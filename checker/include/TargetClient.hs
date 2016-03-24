{-
  1. pc -> # of program counters
  2.
-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import SymMap
import SymVerify hiding (check)

import Text.Printf
import Test.QuickCheck
import Data.Aeson
import Data.Function
import Data.Ix
import Data.List
import Control.Monad
import Data.Map.Strict (findWithDefault)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Set as S
import Control.Parallel.Strategies
import Control.DeepSeq

data AIntOp = AIntEq -- =
            | AIntLt -- <
            | AIntLe -- <=
            | AIntGt -- >
            | AIntGe -- >=
            deriving (Eq,Ord)

data AInt = AIntSingle StateVar      -- for a single process
          | AIntClass  StateVar AInt -- for a process class (map & index)
          | AConst Int               -- 0,1,...
          deriving (Eq,Ord)

data Atom = IntCmp AInt AIntOp AInt
            deriving (Eq,Ord)

data Pred = AndP { predConjuncts :: [Atom] }
          | NegP { negatedPred   :: Pred   }

data Grammar = Imp { antecedent :: Pred
                   , consequent :: Pred
                   }
               deriving (Eq)

-- implications that have the same antecedent
data CandGroup = CandGroup { groupAntecedent  :: Pred
                           , groupConsequents :: [Pred]
                           }

type Run      = [(State, Pid)]
type QCResult = (State, Either Run Run)

fn        = "states.json"
predCount = 10000


-- ######################################################################
-- Main loop, finding invariants
-- ######################################################################


main :: IO ()
main  = do states <- readStates
           --gs     <- generate (vectorOf predCount grammar_gen)
           let ((pc0,_):_)      = thisPcs
               (_:(pc1,_):_)    = thisPcs
               (((_,k2,_),_):_) = thisAbs
               ((i0,_):_)       = thisInts
               (_:(i1,_):_)     = thisInts
               lhs = AndP [ IntCmp (AIntSingle pc0) AIntEq (AConst (-1))
                          , IntCmp (AIntClass pc1 (AIntSingle k2)) AIntEq (AConst 0) ]
               rhs1 = AndP [IntCmp (AIntSingle i0) AIntLe (AIntSingle i1)]
               rhs2 = AndP [IntCmp (AIntSingle i0) AIntGt (AIntSingle i1)]

               gs1 = Imp lhs  -- (pidR0Pc = -1 ∧ pidR2Pc[pidR2K] = 0)
                         rhs1 -- (xl0 ≤ xl1)
               gs2 = Imp lhs  -- (pidR0Pc = -1 ∧ pidR2Pc[pidR2K] = 0)
                         rhs2 -- (xl0 > xl1)

               gs  = [gs1, gs2]
           let candidates  = groupCandidates gs
               invariants  = fit candidates states
               passed_imps = finalize invariants

           printf "candidates:  %s\n" (show candidates)
           printf "invariants:  %s\n" (show invariants)
           printf "passed_imps: %s\n" (show passed_imps)

           printf "size gs = %d\n" (length passed_imps)
           forM_ passed_imps (putStrLn . show)


readStates :: IO [State]
readStates  =
  do bs <- C.readFile fn
     let bs'      = C.drop 1 $ C.dropWhile (/= '\n') bs
         Just qcs = decode bs' :: Maybe [QCResult]
         states   = concatMap extractStates qcs
     return states

  where extractStates (s,e) = s : case e of
                                    Left r  -> map fst r
                                    Right r -> map fst r


groupCandidates   :: [Grammar] -> [CandGroup]
groupCandidates gs =
  let sameAnt          = groupBy ((==) `on` antecedent) $
                           sortBy (compare `on` antecedent) gs
      combine cs@(c:_) = CandGroup (antecedent c)
                                   ((S.toList . S.fromList) (map consequent cs))
      combine []       = error "empty candidate group consequent"
  in map combine sameAnt


fit              :: [CandGroup] -> [State] -> [CandGroup]
fit cands states  = foldr pruneCandidates cands states


pruneCandidates :: State -> [CandGroup] -> [CandGroup]
pruneCandidates s cands =
  filter (not . isTrivial) $ map (pruneConseqs s) cands


pruneConseqs :: State -> CandGroup -> CandGroup
pruneConseqs s cand@(CandGroup a cs) =
  if check a s
    then cand {groupConsequents = filter (\p -> check p s) cs}
    else cand


finalize      :: [CandGroup] -> [Grammar]
finalize cands = map toGrammar cands
                 where toGrammar (CandGroup a cs) =
                         Imp a (AndP $ (S.toList . S.fromList) (concatMap predConjuncts cs))


isTrivial cand = null (groupConsequents cand)

-- ######################################################################
-- Predicate Generation
-- ######################################################################

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
                    rand_pc (_,n) = let maxPc  = find' n pcCounts
                                        allPcs = (range ((-1),maxPc)) :: [Int]
                                    in elements (AConst <$> allPcs)
                    mkpc (v,n) = if isAbs n
                                    then let k = AIntSingle $ getClassK n
                                         in AIntClass v k
                                    else AIntSingle v

rhs_gen :: Gen Pred
rhs_gen  = do rw_ptrs <- rw_ptr_gen
              others  <- listOf atom_gen
              let atoms = rw_ptrs ++ others
              return (AndP atoms)

rw_ptr_gen :: Gen [Atom]
rw_ptr_gen  = sequence $ map fptr thisPtrs
              where fptr ((r,w),n) = let ops = [AIntGe, AIntLt]
                                     in if isAbs n
                                        then let k = AIntSingle $ getClassK n
                                             in IntCmp (AIntClass r k)
                                                  <$> elements ops
                                                  <*> return (AIntClass w k)
                                        else IntCmp (AIntSingle r)
                                               <$> elements ops
                                               <*> return (AIntSingle w)

atom_gen :: Gen Atom
atom_gen  = do operand1 <- aint_gen
               operand2 <- aint_gen
               if operand1 == operand2
                  then atom_gen
                  else do op <- op_gen
                          return $ IntCmp operand1 op operand2

aint_gen :: Gen AInt
aint_gen  = oneof [abs_gen, ptr_gen, ints_gen]

abs_gen :: Gen AInt
abs_gen  = elements $ map (AIntSingle . fst3 . fst) thisAbs

ptr_gen :: Gen AInt
ptr_gen  = elements pointers
           where pointers         = concatMap getInt thisPtrs
                 getInt ((r,w),n) = map sVarToInt [(r,n),(w,n)]

ints_gen :: Gen AInt
ints_gen  = elements (map sVarToInt thisInts)

op_gen :: Gen AIntOp
op_gen  = elements [AIntEq, AIntLt, AIntLe, AIntGt, AIntGe]

sVarToInt      :: (StateVar,Int) -> AInt
sVarToInt (v,n) = if isAbs n
                  then AIntClass  v (AIntSingle $ getClassK n)
                  else AIntSingle v

-- ######################################################################
-- Helper functions
-- ######################################################################
find' k m = findWithDefault (error $ (show k) ++ " doesn't exist in " ++ (show m))
                            (toInteger k) m

isAbs  :: Int -> Bool
isAbs n = snd $ find' n thisPids

getAbs n    = head $ filter ((== n) . snd) thisAbs
getClassN n = let (c,_,_) = fst (getAbs n) in c
getClassK n = let (_,k,_) = fst (getAbs n) in k

uncurry3 f (x,y,z) = f x y z
fst3 (a,_,_)       = a

-- ######################################################################
-- Predicate Evaluator
-- ######################################################################

evalAInt (AConst i) _       = i
evalAInt (AIntSingle v) s   = (sVarAcc v) s
evalAInt (AIntClass v i) s  = get ((sVarAcc2 v) s) (evalAInt i s)

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
-- Some instances
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

instance Show CandGroup where
  show (CandGroup a cs) = printf "%s → %s" (show a) (show cs)

instance NFData State where
  rnf s = s `seq` ()

instance Eq Pred where
  (AndP l1) == (AndP l2) = (S.fromList l1) == (S.fromList l2)
  (NegP p1) == (NegP p2) = p1 == p2
  _ == _                 = False

instance Ord Pred where
  compare (AndP l1) (AndP l2) = compare (S.fromList l1) (S.fromList l2)
  compare (NegP p1) (NegP p2) = compare p1 p2
  compare (AndP _)  (NegP _)  = LT
  compare a b                 = compare b a

instance Eq StateVar where
  (==) s1 s2 = sVarName s1 == sVarName s2

instance Ord StateVar where
  compare s1 s2 = compare (sVarName s1) (sVarName s2)
