{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TargetClient (testTargetClient) where

import SymVector
import SymMap
import SymVerify hiding (check)
import SymBoilerPlate

import Text.Printf
import Test.QuickCheck
import Data.List
import Control.Monad
import Data.Map.Strict (findWithDefault)

data AIntOp = AIntEq -- =
            | AIntLt -- <
            | AIntLe -- <=
            | AIntGt -- >
            | AIntGe -- >=

data AInt = AIntSingle StateVar      -- for a single process
          | AIntClass  StateVar AInt -- for a process class (map & index)
          | AConst Int               -- 0,1,...

data Atom = IntCmp AInt AIntOp AInt

data Pred = AndP [Atom]
          | NegP Pred

data Grammar = Imp Pred Pred

main :: IO ()
main  = testTargetClient

testTargetClient :: IO ()
testTargetClient  = do g <- generate grammar_gen
                       putStrLn $ show g

grammar_gen :: Gen Grammar
grammar_gen  = Imp <$> lhs_gen <*> rhs_gen

lhs_gen :: Gen Pred
lhs_gen =  do len   <- frequency (zip freqs sizes)
              pc_ts <- shuffle thisPcs >>= return . (take len)

              let pcs = mkpc <$> pc_ts
                  ops = const AIntEq <$> pc_ts

              ns <- sequence (rand_pc <$> pc_ts)

              let as = map (uncurry3 IntCmp) (zip3 pcs ops ns)

              return (AndP as)

              where freqs      = if length thisPcs < 3 then [2,3] else [2,3,1]
                    sizes      = map return [1..3]
                    rand_pc _  = return (AConst (-1))
                    mkpc (v,n) = if isAbs n
                                    then AIntClass  v (AConst (-1))
                                    else AIntSingle v

rhs_gen :: Gen Pred
rhs_gen  = do ptrs <- sequence $ map fptr thisPtrs
              return (AndP ptrs)
              where fptr ((r,w),n) =
                      let ops = [AIntGe, AIntLt]
                      in if isAbs n
                         then let k = AIntSingle $ getClassK n
                              in IntCmp (AIntClass r k)
                                   <$> elements ops
                                   <*> return (AIntClass w k)
                         else IntCmp (AIntSingle r)
                                <$> elements ops
                                <*> return (AIntSingle w)

-- ######################################################################
-- Helper functions
-- ######################################################################
find' k m = findWithDefault (error $ (show k) ++ " doesn't exist in " ++ (show m)) k m

isAbs n = snd $ find' n thisPids

getAbs n    = head $ filter ((== n) . snd) thisAbs
getClassN n = let (c,_,_) = fst (getAbs n) in c
getClassK n = let (_,k,_) = fst (getAbs n) in k

uncurry3 f (x,y,z) = f x y z

-- ######################################################################
-- Predicate Evaluator
-- ######################################################################

evalAInt (AConst i) _       = i
evalAInt (AIntSingle v) s   = (sVarAcc v) s
evalAInt (AIntClass v i) s = get ((sVarAcc2 v) s) (evalAInt i s)

evalOp       :: AIntOp -> (Int -> Int -> Bool)
evalOp AIntEq = (==)
evalOp AIntLt = (<)
evalOp AIntLe = (<=)
evalOp AIntGt = (>)
evalOp AIntGe = (>=)

class Checkable a where
  check :: a -> State -> Bool

instance Checkable Atom where
  check (IntCmp l o r) s = (evalOp o) (evalAInt l s) (evalAInt r s)

instance Checkable Pred where
  check (AndP as) s = and (map (\a -> check a s) as)
  check (NegP p) s  = not (check p s)

instance Checkable Grammar where
  check (Imp l r) s = (not $ check l s) || (check r s)

-- ######################################################################
-- Show instances
-- ######################################################################

instance Show AIntOp where
  show AIntEq = "="
  show AIntLt = "<"
  show AIntLe = "≤"
  show AIntGt = ">"
  show AIntGe = "≥"

instance Show AInt where
  show (AIntSingle v)  = sVarName v
  show (AIntClass v i) = printf "%s[%s]" (sVarName v) (show i)
  show (AConst i)      = show i

instance Show Atom where
  show (IntCmp l o r) = unwords [show l, show o, show r]

instance Show Pred where
  show (AndP as) = if null as then "True"
                              else intercalate " ∧ " (show <$> as)
  show (NegP p)  = printf "¬ (%s)" (show p)

instance Show Grammar where
  show (Imp l r) = printf "%s → %s" (show l) (show r)
