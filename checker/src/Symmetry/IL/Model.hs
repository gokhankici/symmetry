{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
module Symmetry.IL.Model where

import Prelude hiding (and, or)
import Data.Generics
import Data.Char
import Symmetry.IL.AST
import Symmetry.IL.ConfigInfo

-- Values    
unitCons, nullCons, intCons, stringCons, pidCons, leftCons, rightCons, pairCons :: String
unitCons   = "VUnit"
nullCons   = "VUnInit"
intCons    = "VInt"
stringCons = "VString"
pidCons    = "VPid"
rightCons  = "VInR"
leftCons   = "VInL"
pairCons   = "VPair"

valCons :: [String]
valCons = [ unitCons
          , nullCons
          , intCons
          , stringCons
          , pidCons
          , rightCons
          , leftCons
          , pairCons
          ]

-- Constant Names
sched, state, initState, initSched, nondet :: String
state = "state"
sched = "sched"
initState = "state0"
initSched = "sched0"
nondet    = "nonDet"

mapGetFn, mapPutFn, vecGetFn, vecPutFn :: String
mapGetFn = "get"
mapPutFn = "put"
vecGetFn = "getVec"
vec2DGetFn = "getVec2D"
vecPutFn = "setVec"
vec2DPutFn = "setVec2D"

-- Generating Names                    
prefix :: String -> String -> String
prefix x (y:ys) = x ++ (toUpper y : ys)

pid :: Pid -> String
pid (PConc i)      = prefix "pid" . prefix "r" $ show i
pid (PAbs _ (S s)) = prefix "pid" s
pid _ = error "pid: non conc or abs"

pidUnfold :: Pid -> String
pidUnfold p@(PAbs _ _) = prefix (pid p) "k"

pidIdx :: Pid -> String
pidIdx (PAbs (V v) _)  = v
pidIdx (PAbs (GV v) _) = v
pidIdx _               = error "pidIdx of non-abs"

pc :: Pid -> String
pc p = prefix (pid p) "pc"

pidPcCounter :: Pid -> String
pidPcCounter p = prefix (pid p) "pcK"

pidPre          = "Pid_pre"

pidTyName ci p t s
  = prefix (pid p) . prefix s $ show tid
  where
    tid = lookupTy ci t

buf :: ConfigInfo Int -> Pid -> ILType -> String
buf ci p t = pidTyName ci p t "buf"

ptrR :: ConfigInfo Int -> Pid -> ILType -> String           
ptrR ci p t = pidTyName ci p t "ptrR"

ptrW :: ConfigInfo Int -> Pid -> ILType -> String           
ptrW ci p t = pidTyName ci p t "ptrW"

data Rule e = Rule Pid e e --[(String, e)], [((Pid, ILType), e)])
              deriving Show

class ILModel e where
  true     :: e
  false    :: e
  expr     :: Pid -> ILExpr -> e
  add       :: e -> e -> e
  incr      :: e -> e
  decr      :: e -> e
  eq        :: e -> e -> e
  and       :: e -> e -> e
  or        :: e -> e -> e
  lneg      :: e -> e
  lt        :: e -> e -> e
  lte       :: e -> e -> e
  int       :: Int -> e
  readPC    :: ConfigInfo Int -> Pid -> e
  readPCCounter :: ConfigInfo Int -> Pid -> e -> e
  readPtrR  :: ConfigInfo Int -> Pid -> ILType -> e
  readPtrW  :: ConfigInfo Int -> Pid -> Pid -> ILType -> e
  readRoleBound :: ConfigInfo Int -> Pid -> e
  readState :: ConfigInfo Int -> Pid -> String -> e
  nonDet    :: ConfigInfo Int -> Pid -> e
  matchVal  :: ConfigInfo Int -> Pid -> e -> [(ILExpr, e)] -> e

  -- updates
  joinUpdate :: ConfigInfo Int -> Pid -> e -> e -> e
  setPC      :: ConfigInfo Int -> Pid -> e -> e
  incrPtrR   :: ConfigInfo Int -> Pid -> ILType -> e
  incrPtrW   :: ConfigInfo Int -> Pid -> Pid -> ILType -> e
  setState   :: ConfigInfo Int -> Pid -> [(String , e)] -> e
  putMessage :: ConfigInfo Int -> Pid -> Pid -> (ILExpr, ILType) -> e
  getMessage :: ConfigInfo Int -> Pid -> (Var, ILType) -> e

  -- rule
  rule     :: ConfigInfo Int -> Pid -> e -> e -> Rule e

  printModel :: ConfigInfo Int -> [Rule e] -> String
                
-------------------------
-- "Macros"
-------------------------
ands :: ILModel e => [e] -> e
ands []     = true
ands [x]    = x
ands (x:xs) = foldr and x xs

ors :: ILModel e => [e] -> e
ors []     = false
ors [x]    = x
ors (x:xs) = foldr or x xs

pcGuard :: ILModel e => ConfigInfo Int -> Pid -> Stmt Int -> e
pcGuard ci p s = readPC ci p `eq` int (annot s)

nextPC :: ILModel e => ConfigInfo Int -> Pid -> Stmt Int -> e
nextPC ci p s = nextPC' ci p $ cfgNext ci p (annot s)
  where
    nextPC' ci p Nothing
      = setPC ci p (int (-1))
    nextPC' ci p (Just [s])
      = setPC ci p (int (annot s))

-------------------------
-- Statement "semantics"    
-------------------------
seqUpdates :: ILModel e => ConfigInfo Int -> Pid -> [e] -> e
seqUpdates ci p = foldl1 (joinUpdate ci p)

ruleOfStmt :: ILModel e => ConfigInfo Int -> Pid -> Stmt Int -> [Rule e]
-------------------------
-- p!e::t
-------------------------
ruleOfStmt ci p s@SSend { sndPid = EPid q, sndMsg = (t, e) }
  = [rule ci p grd (seqUpdates ci p upds)]
  where
    grd  = pcGuard ci p s
    upds = [ incrPtrW ci p q t
           , putMessage ci p q (e, t)
           , nextPC ci p s
           ]
    
ruleOfStmt ci p s@SSend { sndPid = x@EVar {} , sndMsg = (t, e) }
  = [ rule ci p grd (matchVal ci p (expr p x) [(EPid q, ups q) | q <- pids ci]) ]
  -- = concat [ matchVal ci p x (EPid q) <$> ruleOfStmt ci p (sub q) | q <- pids ci ]
    where
      grd = pcGuard ci p s
      ups q = seqUpdates ci p [ incrPtrW ci p q t
                              , putMessage ci p q (e, t)
                              , nextPC ci p s
                              ]
      -- sub q = s { sndPid = EPid q }
              
-------------------------
-- ?(v :: t)
-------------------------
ruleOfStmt ci p s@SRecv{ rcvMsg = (t, v) }
  = [ rule ci p grd (seqUpdates ci p upds) ]
  where
    grd  = pcGuard ci p s `and`
           (readPtrR ci p t `lt` readPtrW ci p p t)
    upds = [ incrPtrR ci p t
           , getMessage ci p (v,t)
           , nextPC ci p s
           ]
    
-------------------------
-- case x of ...    
-------------------------
ruleOfStmt ci p s@SCase{caseLPat = V l, caseRPat = V r}
  = [ rule ci p grd (matchVal ci p (expr p $ EVar (caseVar s)) cases)]
  where
    grd = pcGuard ci p s
    mkLocal v = EVar (V ("local_" ++ v))
    cases = [ (ELeft (mkLocal l), lUpdates)
            , (ERight (mkLocal r), rUpdates)
            ]
    lUpdates = seqUpdates ci p [ setState ci p [(l, expr p $ mkLocal l)]
                               , setPC ci p (int $ annot (caseLeft s))
                               ]
    rUpdates = seqUpdates ci p [ setState ci p [(r, expr p $ mkLocal r)]
                               , setPC ci p (int $ annot (caseRight s))
                               ]
-------------------------
-- nondet choice
-------------------------
ruleOfStmt ci p s@SNonDet{}
  = [ rule ci p (grd s') (us s') | s' <- nonDetBody s ]
  where
    grd s' = pcGuard ci p s `and`
            (nonDet ci p `eq` (int (annot s')))
    us s'  = setPC ci p (int (annot s'))

-------------------------
-- for (i < n) ...
-------------------------
ruleOfStmt ci p s@SIncr { incrVar = (V v), annot = a }
  = [ rule ci p b (seqUpdates ci p upds) ]
  where
    b    = pcGuard ci p s
    upds = [ setState ci p [(v, incr (readState ci p v))]
           , nextPC ci p s
           ]
    
ruleOfStmt ci p s@SIter { iterVar = (V v), iterSet = set, annot = a }
  = [ rule ci p b    (seqUpdates ci p loopUpds)
    , rule ci p notb (seqUpdates ci p exitUpds)
    ]
  where
    b        = pcGuard ci p s `and` lt ve se
    notb     = pcGuard ci p s `and` lte se ve
    loopUpds = [ setPC ci p (int i) ]
    exitUpds = [ setPC ci p (int j) ]
    ve       = readState ci p v
    se       = case set of
                 S s'    -> readState ci p s'
                 SInts n -> int n
    (i, j)   = case (annot <$>) <$> cfgNext ci p a of
                 Just [i, j] -> (j, i)
                 Just [i]    -> (i, -1)
-------------------------
-- catch all
-------------------------
ruleOfStmt ci p s
  = [ rule ci p (pcGuard ci p s) (nextPC ci p s) ]

ruleOfProc :: (ILModel e) => ConfigInfo Int -> Process Int -> [Rule e]
ruleOfProc c (p, s)
  = concatMap (ruleOfStmt c p) ss
  where
    ss = listify (const True) s

rulesOfConfigInfo :: ILModel e => ConfigInfo Int -> [Rule e]
rulesOfConfigInfo c
  = concatMap (ruleOfProc c) ps
  where
    ps = cProcs (config c)

generateModel :: ILModel e => Config () -> (ConfigInfo Int, [Rule e])
generateModel c
  = (cinfo, rulesOfConfigInfo cinfo)
  where
    cinfo = mkCInfo c { cProcs = (freshStmtIds <$>) <$> cProcs c }