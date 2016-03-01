{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TargetClient (testTargetClient) where

import SymVector
import SymMap
import SymVerify
import SymBoilerPlate
import SymQCTH

import Test.QuickCheck
import Data.List
import Control.Monad
import Language.Haskell.TH hiding (Pred)

{-
    1. grammar
       (pc_i = PCI) ∧ ... => ...
    2. generate type from grammar
       ∀ s ∈ states. ∃ t ∈ types. s ∈ t
-}

data AIntOp = AIntEq -- =
            | AIntGt -- >
            | AIntGe -- >=
            deriving (Show)

data AIntV = APC  String     -- Int
           | AVar String     -- Int
           | AVal String     -- Val Pid
           | APtr String Int -- Int
           deriving (Show)

data AInt = AIntSingle AIntV     -- for a single process
          | AIntClass  AIntV Int -- for a process class
          | AConst Int           -- 0 or 1
          deriving (Show)

data Atom = IntCmp AInt AIntOp AInt
          deriving (Show)

data Pred = AtomP Atom
          | AndP  Pred Pred
          | NegP  Pred
          deriving (Show)

data Grammar = Imp Pred Pred
             deriving (Show)

isAbs n = let (_,_,b) = head $ filter (\(_,n',_) -> n' == n) thisPids
          in b

uncurry3 f (x,y,z) = f x y z

testTargetClient :: IO ()
testTargetClient  = do let [(s,ts)] = $(testTH ''State)
                       forM_ ts (putStrLn . show)
                       pred <- generate lhs_gen
                       putStrLn $ show pred
                       return ()

lhs_gen :: Gen Pred
lhs_gen =  do len   <- frequency (zip freqs sizes)
              pc_ts <- shuffle thisPcs >>= return . (take len)

              let pcs = mkpc <$> pc_ts
                  ops = const AIntEq <$> pc_ts
                  ns  = AConst . (const (-1)) <$> pc_ts
                  h:t = map (uncurry3 IntCmp) (zip3 pcs ops ns)

              return $ foldl' (\prev p -> AndP prev (AtomP p)) (AtomP h) t

              where freqs = if length thisPcs < 3 then [2,3] else [2,3,1]
                    sizes = map return [1..3]
                    mkpc (s,n) = if isAbs n
                                    then AIntSingle (APC s)
                                    else AIntClass  (APC s) 0

main :: IO ()
main  = testTargetClient
