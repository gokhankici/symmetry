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

import Text.Printf
import Test.QuickCheck
import Data.List
import Control.Monad
import Language.Haskell.TH hiding (Pred)

-- ######################################################################
-- Template Haskell Functions
-- ######################################################################

$(mkStateAccessor)

{-
    1. grammar
       (pc_i = PCI) ∧ ... => ...
    2. generate type from grammar
       ∀ s ∈ states. ∃ t ∈ types. s ∈ t
-}

data AIntOp = AIntEq -- =
            | AIntLt -- <
            | AIntLe -- <=
            | AIntGt -- >
            | AIntGe -- >=

data AInt = AIntSingle String (State -> Int)                -- for a single process
          | AIntClass  String (State -> Map_t Int Int) AInt -- for a process class (map & index)
          | AConst Int                                      -- 0,1,...

data Atom = IntCmp AInt AIntOp AInt

data Pred = AndP [Atom]
          | NegP Pred

data Grammar = Imp Pred Pred

main :: IO ()
main  = testTargetClient

testTargetClient :: IO ()
testTargetClient  = return ()

-- testTargetClient :: IO ()
-- testTargetClient  = do let [(s,ts)] = $(testTH ''State)
--                        --forM_ ts (putStrLn . show)
--                        pred <- generate grammar_gen
--                        temp
--                        putStrLn $ show pred
--                        return ()

-- grammar_gen :: Gen Grammar
-- grammar_gen  = Imp <$> lhs_gen <*> rhs_gen

-- lhs_gen :: Gen Pred
-- lhs_gen =  do len   <- frequency (zip freqs sizes)
--               pc_ts <- shuffle thisPcs >>= return . (take len)

--               let pcs = mkpc <$> pc_ts
--                   ops = const AIntEq <$> pc_ts

--               ns <- sequence (rand_pc <$> pc_ts)

--               let as = map (uncurry3 IntCmp) (zip3 pcs ops ns)

--               return (AndP as)

--               where freqs      = if length thisPcs < 3 then [2,3] else [2,3,1]
--                     sizes      = map return [1..3]
--                     rand_pc _  = return (AConst (-1))
--                     mkpc (s,n) = if isAbs n
--                                     then AIntClass  (APC s) (AConst (-1))
--                                     else AIntSingle (APC s)

-- rhs_gen :: Gen Pred
-- rhs_gen  = do ptrs <- sequence $ map fptr thisPtrs
--               return (AndP ptrs)
--               where fptr (r,w,n) =
--                       let ops = [AIntGe, AIntLt]
--                       in if isAbs n
--                          then let k = AIntSingle $ AVar $ getClassK n
--                               in IntCmp (AIntClass (APtr r) k)
--                                    <$> elements ops
--                                    <*> return (AIntClass (APtr w) k)
--                          else IntCmp (AIntSingle $ APtr r)
--                                 <$> elements ops
--                                 <*> return (AIntSingle $ APtr w)

-- genOp = elements [AIntEq, AIntLt, AIntLe, AIntGt, AIntGe]

-- lhs_int :: Gen AInt
-- lhs_int  = return (AConst 0)

-- rhs_int :: Gen AInt
-- rhs_int  = return (AConst 0)

-- -- ######################################################################
-- -- Show instances
-- -- ######################################################################

-- instance Show AIntOp where
--   show AIntEq = "="
--   show AIntLt = "<"
--   show AIntLe = "≤"
--   show AIntGt = ">"
--   show AIntGe = "≥"

-- instance Show AInt where
--   show (AIntSingle a)  = sVarName a
--   show (AIntClass a i) = printf "%s[%s]" (show $ AIntSingle a) (show i)
--   show (AConst i)      = show i

-- instance Show Atom where
--   show (IntCmp l o r) = unwords [show l, show o, show r]

-- instance Show Pred where
--   show (AndP as) = if null as then "True"
--                               else intercalate " ∧ " (show <$> as)
--   show (NegP p)  = printf "¬ (%s)" (show p)

-- instance Show Grammar where
--   show (Imp l r) = printf "%s → %s" (show l) (show r)
