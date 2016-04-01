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
import Data.Maybe
import Control.Monad
import Data.Map.Strict (findWithDefault)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import Control.Parallel.Strategies
import Control.DeepSeq
import qualified Text.PrettyPrint.Leijen as P

import Debug.Trace

data AIntOp = AIntEq -- =
            | AIntLt -- <
            | AIntLe -- <=
            | AIntGt -- >
            | AIntGe -- >=
            deriving (Eq,Ord)

data AInt = AIntSingle { svar :: StateVar}
          | AIntClass  { svar :: StateVar, classSize :: StateVar }
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
data CandGroup = CandGroup { groupAntecedent  :: !Pred
                           , groupConsequents :: ![Pred]
                           , touchedGroup     :: !Bool
                           }

type Run      = [(State, Pid)]
type QCResult = (State, Either Run Run)

fn        = "states.json"
predCount = 50000


-- ######################################################################
-- Main loop, finding invariants
-- ######################################################################


main :: IO ()
main  = do bs <- C.readFile fn
           gs <- generate (vectorOf predCount grammar_gen)
           let (n,bs') = extractStateCount bs
               preds   = filterGrammars n bs' gs

           printf "size of predicates %d\n" (length preds)
           forM_ preds (\i -> do P.putDoc $ P.pretty i
                                 printf "\n\n")


extractStateCount :: C.ByteString -> (Int, C.ByteString)
extractStateCount bs =
  let bs1           = bsDropLine bs
      Just size_obj = decode (bsGetLine bs1) :: Maybe (HM.HashMap String Int)
      rest          = applyN 3 bsDropLine bs1
      def           = error "State count not found"
      n             = HM.lookupDefault def "stateCount" size_obj
  in (n, rest)


filterGrammars :: Int
               -> C.ByteString
               -> [Grammar]
               -> [Grammar]
filterGrammars n bs gs =
  finalize $ fst $ filterGrammarsHelper n (groupCandidates gs,bs)

filterGrammarsHelper :: Int
                     -> ([CandGroup], C.ByteString)
                     -> ([CandGroup], C.ByteString)
filterGrammarsHelper 0 t = t
filterGrammarsHelper n t = (filterGrammarsHelper $! (n-1)) $! (partialFilter t)


partialFilter           :: ([CandGroup], C.ByteString)
                        -> ([CandGroup], C.ByteString)
partialFilter (cands,bs) = let (states, rest) = readState bs
                               lhs = fit cands states
                           in  lhs `seq` rest `seq` (lhs,rest)


groupCandidates   :: [Grammar] -> [CandGroup]
groupCandidates gs =
  let sameAnt          = groupBy ((==) `on` antecedent) $
                           sortBy (compare `on` antecedent) gs
      combine cs@(c:_) = CandGroup (antecedent c)
                                   ((S.toList . S.fromList) (map consequent cs))
                                   False
      combine []       = error "empty candidate group consequent"
  in map combine sameAnt


fit              :: [CandGroup] -> [State] -> [CandGroup]
fit cands states  = let newCands = foldl' pruneCandidates cands states
                        r = filter touchedGroup $! newCands
                    in  seq r r


pruneCandidates :: [CandGroup] -> State -> [CandGroup]
pruneCandidates cands s =
  filter (not . isTrivial) $! map (\c -> let r = pruneConseqs s c
                                         in  seq r r)
                                  cands


pruneConseqs :: State -> CandGroup -> CandGroup
pruneConseqs s cand =
  if check (groupAntecedent cand) s
    then cand { groupConsequents = filter (\p -> let r = check p s
                                                 in  seq r r)
                                          (groupConsequents cand)
              , touchedGroup     = True }
    else cand


finalize      :: [CandGroup] -> [Grammar]
finalize cands = map toGrammar cands
                 where toGrammar cand =
                         Imp (groupAntecedent cand)
                             (AndP $ (S.toList . S.fromList)
                                       (concatMap predConjuncts (groupConsequents cand)))


isTrivial cand = null (groupConsequents cand)


-- Read an element from a list
readState :: C.ByteString -> ([State], C.ByteString)
readState bs  =
  let
      bs'        = bsGetLine $ C.dropWhile (== ',') bs -- drop the first comma if it exists
      rest       = bsDropLine bs

      extractStates (s,e) = (:) s $ case e of
                                      Left r  -> map fst r
                                      Right r -> map fst r
  in ( extractStates $ fromJust $ (decode bs' :: Maybe QCResult)
     , rest
     )


-- ######################################################################
-- Predicate Generation


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
                                    then let k = getClassN n
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
                                        then let k = getClassN n
                                             in IntCmp (AIntClass r k)
                                                  <$> elements ops
                                                  <*> return (AIntClass w k)
                                        else IntCmp (AIntSingle r)
                                               <$> elements ops
                                               <*> return (AIntSingle w)

atom_gen :: Gen Atom
atom_gen  = do op1 <- aint_gen
               op2 <- suchThat aint_gen (cond op1)
               op  <- op_gen
               return $ IntCmp op1 op op2
               where cond  op1 op2 = and [c op2 | c <- map ($ op1) conds]
                     conds         = [cond1, cond2]
                     cond1 op1 op2 = op1 /= op2
                     cond2 op1 op2 = case (op1,op2) of
                                       (AConst _, AConst _) -> False
                                       _                    -> True


aint_gen :: Gen AInt
aint_gen  = oneof [abs_gen, ptr_gen, ints_gen, const_gen]

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

const_gen :: Gen AInt
const_gen =  AConst <$> elements [(-1), 0, 1]

sVarToInt      :: (StateVar,Int) -> AInt
sVarToInt (v,n) = if isAbs n
                  then AIntClass  v (getClassN n)
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

uncurry3 f (x,y,z) = f x y z
fst3 (a,_,_)       = a

bsGetLine :: C.ByteString -> C.ByteString
bsGetLine  = C.takeWhile (/= '\n')

bsDropLine   :: C.ByteString -> C.ByteString
bsDropLine bs = C.drop 1 $ C.dropWhile (/= '\n') bs

applyN      :: Int -> (a -> a) -> a -> a
applyN n f a = foldl' (\a' _ -> f $ a') a [1..n]

-- ######################################################################
-- Predicate Evaluator
-- ######################################################################

evalAInt (AConst i) _       = [i]
evalAInt (AIntSingle v) s   = [sVarAcc v s] -- trace (show v) (sVarAcc v s)
evalAInt (AIntClass v n) s  = [get (sVarAcc2 v s) i | i <- [0..(sVarAcc n s)-1]]

evalOp       :: AIntOp -> (Int -> Int -> Bool)
evalOp AIntEq = (==)
evalOp AIntLt = (<)
evalOp AIntLe = (<=)
evalOp AIntGt = (>)
evalOp AIntGe = (>=)

class Checkable a where
  check :: a -> State -> Bool

instance Checkable Atom where
  check (IntCmp l o r) s = all (uncurry (evalOp o)) [(a,b) | a <- ll, b <- rl]
                           where ll = evalAInt l s
                                 rl = evalAInt r s

instance Checkable Pred where
  check (AndP as) s = and (map (\a -> check a s) as)
  check (NegP p) s  = not (check p s)

instance Checkable Grammar where
  check (Imp l r) s = (not $ check l s) || (check r s)

-- ######################################################################
-- Some instances
-- ######################################################################

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


-- ######################################################################
-- Pretty
-- ######################################################################

_pretty = P.align . P.pretty

instance P.Pretty AIntOp where
  pretty AIntEq = P.text "="
  pretty AIntLt = P.text "<"
  pretty AIntLe = P.text "≤"
  pretty AIntGt = P.text ">"
  pretty AIntGe = P.text "≥"

instance P.Pretty AInt where
  pretty (AIntSingle v)  = P.text (sVarName v)
  pretty (AIntClass v n) = (P.text $ sVarName v) P.<> (P.brackets $ P.text "i")
  pretty (AConst i)      = P.int i

instance P.Pretty Atom where
  pretty (IntCmp l o r) = P.hsep [P.pretty l, P.pretty o, P.pretty r]

instance P.Pretty Pred where
  pretty (AndP as) = if null as then P.text "True"
                                else P.align $ P.sep
                                             $ P.punctuate (P.text " ∧") (P.pretty <$> as)
  pretty (NegP p)  = (P.text "¬") P.<+> P.parens (P.pretty p)

instance P.Pretty Grammar where
  pretty (Imp l r) = P.fill 20 (_pretty l) P.<+> P.text "→"
                                           P.<+> _pretty r

instance Show StateVar where
  show = sVarName

-- ######################################################################
-- Test
-- ######################################################################


-- testGrammarFilter states =
--   let ((pc0,_):_)      = thisPcs
--       (_:(pc1,_):_)    = thisPcs
--       (((k2,_,_),_):_) = thisAbs
--       ((i0,_):_)       = thisInts
--       (_:(i1,_):_)     = thisInts
--       lhs = AndP [ IntCmp (AIntSingle pc0) AIntEq (AConst (-1))
--                  , IntCmp (AIntClass pc1 k2) AIntEq (AConst 0) ]
--       rhs1 = AndP [IntCmp (AIntSingle i0) AIntLe (AIntSingle i1)]
--       rhs2 = AndP [IntCmp (AIntSingle i0) AIntGt (AIntSingle i1)]

--       gs1 = Imp lhs  -- (pidR0Pc = -1 ∧ pidR2Pc[pidR2K] = 0)
--                 rhs1 -- (xl0 ≤ xl1)
--       gs2 = Imp lhs  -- (pidR0Pc = -1 ∧ pidR2Pc[pidR2K] = 0)
--                 rhs2 -- (xl0 > xl1)
--       gs  = [gs1, gs2]
--   in filterGrammars gs states
