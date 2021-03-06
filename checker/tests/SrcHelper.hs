{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ImplicitParams #-}
module SrcHelper where

import Symmetry.Language
import Symmetry.SymbEx

import Prelude hiding ((>>=), (>>), fail, return, id, mod)
import Data.Typeable
import GHC.Stack

id :: DSL repr => repr (a-> Process repr a)
id  = lam $ \x -> ret x

reject :: DSL repr => repr (a-> Process repr b)
reject  = lam $ \_ -> fail

app2 :: DSL repr => repr (a->b->c) -> repr a -> repr b -> repr c
app2 f a1 a2 = app (app f a1) a2

app3 :: DSL repr => repr (a->b->c->d) -> repr a -> repr b -> repr c -> repr d
app3 f a1 a2 a3 = app (app (app f a1) a2) a3

app4 :: DSL repr => repr (a->b->c->d->e) -> repr a -> repr b -> repr c -> repr d -> repr e
app4 f a1 a2 a3 a4 = app (app (app (app f a1) a2) a3) a4

app5 :: DSL repr
     => repr (a->b->c->d->e->f)
     -> repr a -> repr b -> repr c -> repr d -> repr e -> repr f
app5 f a1 a2 a3 a4 a5 = app (app (app (app (app f a1) a2) a3) a4) a5

ifte      :: ( ?callStack :: CallStack
             , DSL repr
             , ArbPat repr ()
             )
          => repr Boolean -> repr a -> repr a -> repr a
ifte b t e = match b (lam $ \_ -> t) (lam $ \_ -> e)

type T_lookup a b = (a, ([(a,b)], (Either () b)))

f_lookup :: ( ?callStack :: CallStack
            , DSL repr
            , Typeable a, Typeable b
            , ArbPat repr a, ArbPat repr b
            , Ord a
            )
         => repr ((T_lookup a b -> Process repr (T_lookup a b))
                  -> T_lookup a b -> Process repr (T_lookup a b))
f_lookup  = lam $ \lookup -> lam $ \arg ->
              let k = proj1 arg
                  m = proj1 $ proj2 arg
                  r = proj2 $ proj2 arg
               in matchList m (lam $ \_  -> ret $ pair3 k m (inl tt))
                              (lam $ \ht -> let x  = proj1 ht
                                                tl = proj2 ht
                                                k' = proj1 x
                                                v' = proj2 x
                                             in ifte (eq k k')
                                                  (ret $ pair3 k m (inr v'))
                                                  (app lookup $ pair3 k tl r))

lookup :: ( ?callStack :: CallStack
          , DSL repr
          , Ord a
          , Typeable a, Typeable b
          , ArbPat repr a, ArbPat repr b
          )
       => repr (a -> [(a,b)] -> Process repr (Either () b))
lookup  = lam $ \k -> lam $ \m ->
            do r <- app (fixM f_lookup) (pair3 k m (inl tt))
               ret $ proj2 $ proj2 r

print :: DSL repr => repr (a -> Process repr ())
print  = lam $ \_ -> ret tt

mod :: ( ?callStack :: CallStack
       , DSL repr
       ) => repr (Int -> Int -> Process repr Int)
mod  = lam $ \a -> lam $ \b ->
         do let f_mod = lam $ \mod -> lam $ \ab ->
                          do let a = proj1 ab
                             let b = proj2 ab
                             ifte (lt a b)
                                 (ret $ pair a b)
                                 (app mod $ pair (plus a (neg b)) b)
            r <- app (fixM f_mod) $ pair a b
            ret $ proj1 r

match3 :: ( ?callStack :: CallStack
          , DSL repr
          , Typeable a, Typeable b, Typeable c
          , ArbPat repr a, ArbPat repr b, ArbPat repr c
          )
       => repr (Either a (Either b c))
       -> repr (a -> r) -> repr (b -> r) -> repr (c -> r)
       -> repr r
match3 msg f1 f2 f3 = match msg f1 $ lam (\e1 -> match e1 f2 f3)

match4 :: ( ?callStack :: CallStack
          , DSL repr
          , Typeable a, Typeable b, Typeable c, Typeable d
          , ArbPat repr a, ArbPat repr b, ArbPat repr c, ArbPat repr d
          )
       => repr (Either a (Either b (Either c d)))
       -> repr (a -> r) -> repr (b -> r) -> repr (c -> r) -> repr (d -> r)
       -> repr r
match4 msg f1 f2 f3 f4 = match msg f1 . lam $ \e1 ->
                           match e1 f2 . lam $ \e2 ->
                             match e2 f3 f4

match5 :: ( ?callStack :: CallStack
          , DSL repr
          , Typeable a, Typeable b, Typeable c, Typeable d, Typeable e
          , ArbPat repr a, ArbPat repr b, ArbPat repr c, ArbPat repr d, ArbPat repr e
          )
       => repr (Either a (Either b (Either c (Either d e))))
       -> repr (a -> r) -> repr (b -> r) -> repr (c -> r) -> repr (d -> r) -> repr (e -> r)
       -> repr r
match5 msg f1 f2 f3 f4 f5 = match msg f1 $ lam $ \e1 ->
                             match e1 f2 $ lam $ \e2 ->
                               match e2 f3 $ lam $ \e3 ->
                                 match e3 f4 f5

compare :: ( ?callStack :: CallStack
           , DSL repr
           )
        => repr (Int -> Int -> (Either () (Either () ())))
compare  = lam $ \x -> lam $ \y -> ifte (lt x y) (inl tt)
                                     (ifte (eq x y) (inr $ inl tt) (inr $ inr tt))

pair3 :: ( DSL repr
         )
      => repr a -> repr b -> repr c -> repr (a,(b,c))
pair3 a1 a2 a3 = pair a1 (pair a2 a3)

pair4 :: ( DSL repr
         )
      => repr a -> repr b -> repr c -> repr d -> repr (a,(b,(c,d)))
pair4 a1 a2 a3 a4 = pair a1 $ pair a2 $ pair a3 a4

pair5 :: ( DSL repr
         )
      => repr a -> repr b -> repr c -> repr d -> repr e -> repr (a,(b,(c,(d,e))))
pair5 a1 a2 a3 a4 a5 = pair a1 $ pair a2 $ pair a3 $ pair a4 a5
