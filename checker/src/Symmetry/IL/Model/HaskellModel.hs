{-# Language ParallelListComp #-}
{-# Language TupleSections #-}
module Symmetry.IL.Model.HaskellModel where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Function
import           Text.Printf
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax hiding (Rule, EVar)
import           Language.Haskell.Exts.Build
import           Language.Haskell.Exts.Pretty
import           Symmetry.IL.AST
import           Symmetry.IL.Model
import           Symmetry.IL.ConfigInfo
import           Symmetry.IL.Model.HaskellDefs
import           Symmetry.IL.Model.HaskellDeadlock
import           Symmetry.IL.Model.HaskellSpec ( initSpecOfConfig
                                               , valHType
                                               , intType
                                               , mapType
                                               , withStateFields
                                               )

import Debug.Trace

---------------------------------
-- Wrapper type for Haskell code
---------------------------------
data HaskellModel = ExpM { unExp :: Exp }
                  | PatM { unPat :: Pat }
                  | StateUpM [(String, Exp)] [((Pid, ILType), Exp)]
                  | GuardedUpM Exp [(ILExpr, HaskellModel)]
                    deriving Show

---------------------
-- Convenience Functions                    
---------------------
vExp :: String -> Exp
vExp = var . name

opEq, opAnd, opOr, opPlus, opMinus, opLt, opLte, opGt, opGte :: QOp
opEq   = op (sym "==")
opAnd  = op (sym "&&")
opOr   = op (sym "||")
opPlus = op (sym "+")
opMinus= op (sym "-")
opLt   = op (sym "<")
opLte  = op (sym "<=")
opGt   = op (sym ">")
opGte  = op (sym ">=")

neg :: Exp
neg = vExp "not"

---------------------
-- Program Expressions
---------------------
getMap :: Exp -> Exp -> Exp
getMap m key
  = paren (app (app (vExp mapGetFn) m) key)

getVec :: Exp -> Exp -> Exp    
getVec vec key 
  = paren (app (app (vExp vecGetFn) key) vec)

getVec2D :: Exp -> Exp -> Exp -> Exp
getVec2D vec key1 key2 
  = paren (app (app (app (vExp vec2DGetFn) key1) key2) vec)

putMap :: Exp -> Exp -> Exp -> Exp
putMap m key val
  = paren (app (app (app (vExp mapPutFn) m) key) val)

putVec :: Exp -> Exp -> Exp -> Exp
putVec m key val
  = paren (app (app (app (vExp vecPutFn) key) val) m)

putVec2D :: Exp -> Exp -> Exp -> Exp -> Exp
putVec2D m key1 key2 val
  = paren (app (app (app (app (vExp vec2DPutFn) key1) key2) val) m)

stateUpdate :: [(String, Exp)] -> Exp
stateUpdate us
  = RecUpdate (vExp state) [FieldUpdate (UnQual (name f)) e | (f, e) <- us]
    
-- Implementations
hEq :: HaskellModel -> HaskellModel -> HaskellModel
hEq (ExpM e1) (ExpM e2)
  = ExpM $ paren (infixApp e1 opEq e2)

hLt :: HaskellModel -> HaskellModel -> HaskellModel
hLt (ExpM e1) (ExpM e2)
  = ExpM $ paren (infixApp e1 opLt e2)

hLte :: HaskellModel -> HaskellModel -> HaskellModel
hLte (ExpM e1) (ExpM e2)
  = ExpM $ paren (infixApp e1 opLte e2)

hAnd :: HaskellModel -> HaskellModel -> HaskellModel
hAnd (ExpM e1) (ExpM e2)
  = ExpM $ paren (infixApp e1 opAnd e2)

hOr :: HaskellModel -> HaskellModel -> HaskellModel
hOr (ExpM e1) (ExpM e2)
  = ExpM $ paren (infixApp e1 opOr e2)

hLNeg :: HaskellModel -> HaskellModel
hLNeg (ExpM b)
  = ExpM $ paren (app neg b)

hTrue, hFalse :: HaskellModel    
hTrue  = ExpM (vExp "True")
hFalse = ExpM (vExp "False")

hInt :: Int -> HaskellModel
hInt x
  = ExpM $ if x < 0 then paren (mkInt x) else mkInt x
  where
    mkInt = intE . toInteger 

hReadState :: ConfigInfo Int
           -> Pid
           -> String
           -> HaskellModel
hReadState _ (PConc _) f
  = ExpM $ vExp f
hReadState _ p f
  = ExpM $ getMap (vExp f) (vExp (pidIdx p))

hReadPC :: ConfigInfo Int
        -> Pid
        -> HaskellModel
hReadPC ci p
  = hReadState ci p (pc p)

hReadPCCounter :: ConfigInfo Int
               -> Pid
               -> HaskellModel
               -> HaskellModel
hReadPCCounter ci p (ExpM i)
  = ExpM $ getMap s i
  where
    s = vExp $ pcCounter p

hReadRoleBound :: ConfigInfo Int
               -> Pid
               -> HaskellModel
hReadRoleBound ci p
 = ExpM . vExp $ pidBound p

hIncr :: HaskellModel -> HaskellModel
hIncr (ExpM e)
  = ExpM $ infixApp e opPlus (intE 1)

hDecr :: HaskellModel -> HaskellModel
hDecr (ExpM e)
  = ExpM $ infixApp e opMinus (intE 1)

hAdd :: HaskellModel -> HaskellModel -> HaskellModel
hAdd (ExpM e1) (ExpM e2)
  = ExpM $ infixApp e1 opPlus e2

hSetFields :: ConfigInfo Int
           -> Pid
           -> [(String, HaskellModel)]
           -> HaskellModel
hSetFields ci (PConc _) ups
  = StateUpM [ (f, e) | (f, ExpM e) <- ups ] []
hSetFields ci p ups
  = StateUpM [ (f, putMap (vExp f) (vExp $ pidIdx p) e) | (f, ExpM e) <- ups ] []

hSetPC ci p e
  = hSetFields ci p [(pc p, e)]

con :: String -> Exp             
con = Con . UnQual . name

hExpr :: ConfigInfo Int -> Pid -> ILExpr -> HaskellModel
hExpr _ _ EUnit    = ExpM $ con unitCons    
hExpr _ _ EString  = ExpM $ con stringCons
hExpr _ _ (EInt i) = hInt i
hExpr _ _ (EPid q@(PConc _))
  = ExpM $ App (con pidCons) (vExp $ pidConstructor q)
hExpr ci p (EPid q@(PAbs (V v) _))
  = ExpM $ App (con pidCons) (App (vExp $ pidConstructor q) (unExp $ readState ci p v))
hExpr _ _ (EPid q@(PAbs _ _))
  = ExpM $ App (con pidCons) (App (vExp $ pidConstructor q) (vExp $ pidIdx q))
hExpr _ _ (EVar (GV v))
  = ExpM . vExp $ v
hExpr ci p (EVar (VPtrR t))
  = readPtrR ci p t
hExpr ci p (EVar (VPtrW t))
  = readPtrW ci p p t
hExpr ci p (EVar (V v))
  = hReadState ci p v
hExpr ci _ (EVar (VRef q v))
  = hReadState ci q v
hExpr ci p (ELeft e)
  = ExpM $ App (con leftCons) (unExp $ hExpr ci p e)
hExpr ci p (ERight e)
  = ExpM $ App (con rightCons) (unExp $ hExpr ci p e)
hExpr ci p (EPair e1 e2)
  = ExpM $ App (App (con pairCons) (unExp $ hExpr ci p e1))
               (unExp $ hExpr ci p e2)
hExpr ci p (EPlus e1 e2)
  = ExpM $ infixApp e1' opPlus e2'
  where
    e1' = unExp $ hExpr ci p e1
    e2' = unExp $ hExpr ci p e2

hExpr ci p e
  = error (printf "hExpr: TBD(%s)" (show e))

hPred :: ConfigInfo Int -> Pid -> ILPred -> HaskellModel
hPred ci _ ILPTrue
  = ExpM $ vExp "True"
hPred ci p (ILNot b)
  = ExpM $ (paren (metaFunction "not" [unExp $ hPred ci p b]))
hPred ci p (ILAnd b1 b2)
  = ExpM $ InfixApp (unExp $ hPred ci p b1) opAnd (unExp $ hPred ci p b2)
hPred ci p (ILOr b1 b2)
  = ExpM $ InfixApp (unExp $ hPred ci p b1) opOr (unExp $ hPred ci p b2)
hPred ci p (ILBop o e1 e2)
  = ExpM $ paren (infixApp (unExp $ hExpr ci p e1)
                           (qop o)
                           (unExp $ hExpr ci p e2))
  where
    qop Eq = opEq
    qop Lt = opLt
    qop Le = opLte
    qop Gt = opGt
    qop Ge = opGte

hReadPtrR ci p t
  = hReadState ci p (ptrR ci p t)

hIncrPtrR ci p t
  = hSetFields ci p [(ptrR ci p t, hIncr ptrRExp)]
  where
    ptrRExp = hReadPtrR ci p t

hIncrPtrW ci p q t
  = hSetFields ci q [(ptrW ci q t, hIncr ptrWExp)]
  where
    ptrWExp = hReadPtrW ci p q t

hReadPtrW ci p q t
  = hReadState ci q (ptrW ci q t)

hPutMessage ci p q (e,t)
  = StateUpM [] [((q, t), e')]
  where
    ExpM e' = hExpr ci p e

hGetMessage ci p@PConc{} (V v, t)
  = hSetFields ci p [(v, ExpM $ getVec (vExp (buf ci p t)) e')]
  where
    ExpM e' = hReadPtrR ci p t

hGetMessage ci p@(PAbs (GV i) _) (V v, t)
  = hSetFields ci p [(v, ExpM $ getVec2D (vExp (buf ci p t)) (vExp i) e')]
  where
    ExpM e' = hReadPtrR ci p t

hMatchVal :: ConfigInfo Int
          -> Pid
          -> HaskellModel
          -> [(ILExpr, HaskellModel)]
          -> HaskellModel
hMatchVal ci p (ExpM f) cases
  | null cases = error "empty cases!"
  | otherwise  = GuardedUpM f cases
-- hMatchVal ci p f (EPid q) (Rule p' ms grd ups)
--   = Rule p' ((f, PatM $ pidPattern q) : ms) grd ups

-- hMatchVal ci p f (ELeft (EVar (V v))) (Rule p' ms grd ups)
--   = Rule p' ((f, PatM $ (PApp (UnQual (name leftCons)) [pvar (name v)])) : ms) grd ups

-- hMatchVal ci p f (ERight (EVar (V v))) (Rule p' ms grd ups)
--   = Rule p' ((f, PatM $ (PApp (UnQual (name rightCons)) [pvar (name v)])) : ms) grd ups

hNonDet :: ConfigInfo Int
        -> Pid
        -> HaskellModel
hNonDet _ _
  = ExpM $ metaFunction nondet [vExp sched]

hNonDetRange :: ConfigInfo Int
             -> Pid
             -> Set
             -> HaskellModel
hNonDetRange ci p (S s)
  = ExpM $ metaFunction nondetRange [intE 0, unExp $ readState ci p s]
hNonDetRange _ _ _
  = error "hNonDetRange (TBD)"

hRule :: ConfigInfo Int
      -> Pid
      -> HaskellModel
      -> Maybe (HaskellModel)
      -> HaskellModel
      -> Rule HaskellModel
hRule _ p g assert us
  = Rule p g assert us

hJoinUpdates _ _ (StateUpM us bufs) (StateUpM us' bufs')
  = StateUpM (us ++ us') (bufs ++ bufs')

instance ILModel HaskellModel where 
  int        = hInt
  add        = hAdd
  incr       = hIncr
  decr       = hDecr
  expr       = hExpr
  pred       = hPred
  lt         = hLt
  lte        = hLte
  eq         = hEq
  and        = hAnd
  or         = hOr
  lneg       = hLNeg
  true       = hTrue
  false      = hFalse
  readState  = hReadState
  readPtrR   = hReadPtrR
  readPtrW   = hReadPtrW
  readPC     = hReadPC
  readPCCounter = hReadPCCounter
  readRoleBound = hReadRoleBound
  nonDet = hNonDet
  nonDetRange = hNonDetRange

  joinUpdate = hJoinUpdates
  setPC      = hSetPC
  setState   = hSetFields
  incrPtrR   = hIncrPtrR
  incrPtrW   = hIncrPtrW
  putMessage = hPutMessage
  getMessage = hGetMessage

  rule       = hRule
  matchVal   = hMatchVal
  printModel = printHaskell
  printCheck = printQCFile


ilExpPat :: ILExpr -> Pat
ilExpPat (EPid q)
  = PApp (UnQual (name pidCons)) [pidPattern q]
ilExpPat (ELeft (EVar v))
  = PApp (UnQual (name leftCons)) [varPat v]
ilExpPat (ERight (EVar v))
  = PApp (UnQual (name rightCons)) [varPat v]
ilExpPat e
  = error (printf "ilExpPath TODO(%s)" (show e))

varPat :: Var -> Pat    
varPat (V v)  = pvar $ name v
varPat (GV v) = pvar $ name v

printRules ci rs dl = prettyPrint $ FunBind matches
  where
    matches = (mkMatch <$> perPid) ++ [ mkDlMatch ]
    mkMatch rules
      = Match noLoc (name runState) (pat [rulesPid rules]) Nothing (mkGuardedRhss rules) Nothing 
    mkGuardedRhss rules
      = GuardedRhss [ mkRhs p grd a up | Rule p (ExpM grd) a up <- rules ]
    mkRhs p grd a (GuardedUpM f cases)
      = GuardedRhs noLoc [Qualifier grd] (Case f (mkAlt p a <$> cases))
    mkRhs p grd a (StateUpM fups bufups)
      = GuardedRhs noLoc [Qualifier grd] (mkCall p a fups bufups)
    -- mkRhs p ms grd fups bufups
    --   = GuardedRhs noLoc [Qualifier grd] (mkCall p fups bufups)
    mkAlt p a (ile, StateUpM fups bufups)
      = Alt noLoc (ilExpPat ile) (UnGuardedRhs (mkCall p a fups bufups)) Nothing
    -- mkCase [(f, PatM p)] e
    --   = Case (vExp f) [Alt noLoc p (UnGuardedRhs e) Nothing]
    mkRecUp p fups
      = RecUpdate (vExp state) [mkFieldUp p f e | (f,e) <- fups]
    mkFieldUp p f e
      = FieldUpdate (UnQual (name f)) e
    mkCall p (Just (ExpM e)) fups bufups
      = mkAssert e (paren (mkCall p Nothing fups bufups))
    mkCall p Nothing fups bufups
      = Let (BDecls [PatBind noLoc
                             (pvar $ name "qc_s'")
                             (UnGuardedRhs $ mkRecUp p fups)
                             Nothing]) $
            metaFunction runState
                         ((var $ name "qc_s'") : mkBufUps bufups ++ [vExp sched] ++
                          (ifQC ci (Paren $ infix_syn ":"
                                                      (var $ name "qc_s'")
                                                      (var $ name "qc_ss"))))
    mkBufUps bufups
      = [ findUp p t bufups | p <- pids ci, t <- fst <$> tyMap ci]

    rulesPid rules = let Rule p _ _ _ = head rules in p
    pidRule (Rule p _ _ _) = p
    eqPid  = (==) `on` pidRule
    perPid = groupBy eqPid $ sortBy (compare `on` pidRule) rs

    pat ps = [ PAsPat (name state) (PRec (UnQual (name stateRecordCons)) [PFieldWildcard]) ] ++
             [ pvar (name (buf ci q t)) | q <- pids ci, t <- fst <$> tyMap ci ] ++
             [ mkSchedPat ps ] ++
             (ifQC ci (pvar (name "qc_ss")))

    mkSchedPat ps = foldr (\q rest -> PInfixApp (pidPattern q) list_cons_name rest) schedPVar ps
    schedPVar  = pvar (name sched)

    mkDlMatch  = Match noLoc (name runState) dlpat Nothing dlRhs Nothing
    dlRhs      = UnGuardedRhs (mkAssert dl (if isQC ci
                                              then (var $ name "qc_ss")
                                              else unit_con))
    dlpat      = pat (pids ci)

    findUp p t bufups
      = maybe (vExp $ buf ci p t) (\(p, e) -> updateBuf ci p t e) $ findUpdate p t bufups

mkAssert e k
  = infixApp (metaFunction "liquidAssert" [e]) (op . sym $ "$") k

updateBuf :: ConfigInfo Int -> Pid -> ILType -> Exp -> Exp
updateBuf ci p@(PAbs _ _) t e
  = putVec2D v i j e 
    where
      v = vExp $ buf ci p t
      i = vExp $ pidIdx p
      j = getMap (vExp $ ptrW ci p t) (vExp $ pidIdx p)
updateBuf ci p t e
  = putVec v i e
  where
    v = vExp $ buf ci p t
    i = vExp $ ptrW ci p t
          

findUpdate :: Pid -> ILType -> [((Pid, ILType), Exp)] -> Maybe (Pid, Exp)
findUpdate (PAbs _ (S s)) t bufups
  = case [ (p, e) | ((p@(PAbs _ (S s')), t'), e) <- bufups, s == s', t == t' ] of
      []  -> Nothing
      x:_ -> Just x
findUpdate p t bufups
  = (p,) <$> lookup (p, t) bufups

undef = "undefined"                               
undefinedExp = vExp undef

initialState :: ConfigInfo Int -> [Decl]
initialState _
  = [ TypeSig noLoc [name initState] (TyCon (UnQual (name stateRecordCons)))
    , nameBind noLoc (name initState) undefinedExp
    ]

initialSched :: ConfigInfo Int -> [Decl]
initialSched _
  = [ TypeSig noLoc [name initSched] fnType
    , simpleFun noLoc (name initSched) (name state) undefinedExp
    ]
  where
    fnType = TyFun (TyCon (UnQual (name stateRecordCons)))
                   schedType

    -- -- Match noLoc (name runState) (pat [rulesPid rules]) Nothing (mkGuardedRhss rules) Nothing
    -- totalRunState = Match noLoc (name runState)
    --                       (pat [rulesPid rules])
    --                       Nothing
    --                       (UnGuardedRhs (Con $ Special UnitCon))
    --                       Nothing

totalCall :: ConfigInfo Int -> Decl
totalCall ci =
  FunBind [Match noLoc (name runState) args Nothing rhs Nothing]
    where
      bufs = [ wildcard | _ <- pids ci, _ <- tyMap ci ]
      args = (wildcard : bufs) ++ [wildcard] ++ (ifQC ci wildcard)
      rhs  = UnGuardedRhs $ if isQC ci then emptyListCon else unitCon

initialCall :: ConfigInfo Int -> Decl
initialCall ci =
  nameBind noLoc (name "check") call
    where
      call = metaFunction runState (vExp initState : bufs ++ [initSchedCall] ++ ss)
      ss = ifQC ci emptyListCon
      bufs = [ emptyVec p | p <- pids ci, _ <- tyMap ci ]
      emptyVec p = vExp $ if isAbs p then "emptyVec2D" else "emptyVec"
      initSchedCall = metaFunction initSched [vExp initState]
          
printHaskell :: ConfigInfo Int -> [Rule HaskellModel] -> String
printHaskell ci rs = unlines [ header
                             , ""
                             , body
                             ]
  where
    header = unlines $ [ "{-# Language RecordWildCards #-}"
                       , "module SymVerify where"
                       , "import SymVector"
                       , "import SymMap"
                       , "import SymBoilerPlate"
                       ] ++ (if isQC ci then [] else ["import Language.Haskell.Liquid.Prelude"])

    ExpM dl   = deadlockFree ci
    body = unlines [ unlines (prettyPrint <$> initialState ci)
                   , unlines (prettyPrint <$> initialSched ci)
                   , prettyPrint (initialCall ci)
                   , printRules ci rs dl
                   , prettyPrint (totalCall ci)
                   , ""
                   , initSpecOfConfig ci
                   ]

-- ######################################################################
-- ### QUICK CHECK ######################################################
-- ######################################################################

printQCFile :: ConfigInfo Int -> [Rule HaskellModel] -> String
printQCFile ci _
  = unlines lhFile
  where
    sep    = concatMap (\s -> [s,""])
    lhFile = sep (map unlines [header, spec])
    header = [ "{-# Language RecordWildCards #-}"
             , "{-# LANGUAGE OverloadedStrings #-}"
             , "module QC () where"
             , "import SymVector"
             , "import SymVector"
             , "import SymMap"
             , "import SymVerify"
             , "import SymBoilerPlate"
             , "import Test.QuickCheck"
             , "import Test.QuickCheck.Monadic"
             , "import Data.Aeson"
             , "import Data.Aeson.Encode.Pretty"
             , "import Control.Monad"
             , "import Data.ByteString.Lazy.Char8 as C (putStrLn, writeFile, readFile)"
             , "import Data.HashMap.Strict as H hiding (map,filter,null)"
             , "import Data.Maybe"
             , "import System.Directory"
             ]
    spec =  qcDefsStr
            : qcMainStr
            : (prettyPrint  $  runTestDecl ci) : ""
            : arbValStr : ""
            : arbVecStr : ""
            : sep (prettyPrint <$> arbitraryDecls ci)
            ++ sep (prettyPrint <$> jsonDecls ci)

emptyListCon = Con $ Special $ ListCon
unitCon      = Con $ Special $ UnitCon

ifQC ci x = if isQC ci then [x] else []

infix_syn sym f g = InfixApp f (QConOp $ UnQual $ Symbol sym) g
fmap_syn          = infix_syn "<$>"
fapp_syn          = infix_syn "<*>"

arbitraryDecls :: ConfigInfo Int -> [Decl]
arbitraryDecls ci = [ arbitraryPidPreDecl ci
                    , arbitraryStateDecl  ci ]

jsonDecls   :: ConfigInfo Int -> [Decl]
jsonDecls ci = ($ ci) <$> [ stateFromJSONDecl
                          , stateToJSONDecl
                          , pidFromJSONDecl
                          , pidToJSONDecl     ]

-- ### QuickCheck helpers ##################################################

-- runTest (s,plist)
--   = let l = runState s emptyVec emptyVec plist []
--     in (reverse l, plist)

-- prop_runState :: State -> [Pid_pre] -> Property
-- prop_runState s plist = monadicIO $ do
--   let l = runState s emptyVec emptyVec plist []
--   in if null l
--         then return ()
--         else run (log_states l)
--   assert True
runTestDecl    :: ConfigInfo Int -> Decl
runTestDecl ci =
  FunBind [ Match noLoc (name "runTest") args Nothing (UnGuardedRhs rhs) Nothing ]
  where pvarn n    = pvar $ name n
        varn n     = Var $ UnQual $ name n
        args       = [PTuple Boxed [pvarn "s"]]
        emptyVec p = vExp $ if isAbs p then "emptyVec2D" else "emptyVec"
        -- turn buffers into empty vectors
        bufs       = [ emptyVec p | p <- pids ci, _ <- tyMap ci ]
        -- runState s ... plist []
        rs_app     = appFun (varn runState)
                            ((varn "s") : bufs ++ [varn "sched", emptyListCon])
        -- let l = runState ... in ...
        rhs = Do [ Generator noLoc (pvarn "sched") schedGen
                 , LetStmt lets
                 , Qualifier (metaFunction "return" [retExp])
                 ]
        schedGen = metaFunction "generate"
                   [ metaFunction "listOf1"
                     [ metaFunction "oneof" [listE [ mkPid p | p <- pids ci ]] ]
                   ]
        mkPid p = if isAbs p then 
                    fmap_syn (pidCons p)
                             (arbRange (intE 0) (pidChoose p))
                  else
                    metaFunction "return" [pidCons p]
        pidChoose p = metaFunction (pidBound p) [vExp "s"]
        pidCons p   = Con . UnQual $ name (pidConstructor p) 
        lets        = BDecls [PatBind noLoc (pvarn "l") (UnGuardedRhs rs_app) Nothing]
                         -- ret_exp
        -- (reverse l, sched)
        retExp    = Tuple Boxed [app (varn "reverse") (varn "l"), (varn "sched")]


-- ### Arbitrary instances ##################################################

-- instance Arbitrary State where
--         arbitrary
--           = State <$> .. <*> ...

arbFn :: String
arbFn  = "arbitrary"

arbPos :: Exp    
arbPos = fmap_syn (vExp "getPositive") (vExp arbFn)

arbZero :: Exp
arbZero = metaFunction "return" [intE 0]

arbRange :: Exp -> Exp -> Exp
arbRange e1 e2 = metaFunction "choose"
                 [App (App (tuple_con Boxed 1) e1) (infixApp e2 opMinus (intE 1))]

arbNull :: Exp
arbNull = metaFunction "return" [vExp nullCons]

arbEmptyMap :: Exp
arbEmptyMap = metaFunction "return" [vExp "emptyMap"]

arbitraryStateDecl    :: ConfigInfo Int -> Decl
arbitraryStateDecl ci =  InstDecl noLoc Nothing [] [] tc_name [tv_name] [InsDecl (FunBind [arb])]
  where tc_name   = UnQual $ name "Arbitrary"
        tv_name   = TyVar $ name stateRecordCons
        var' s    = var $ name s
        arb       = Match noLoc (name "arbitrary") [] Nothing genExp Nothing
        genExp    = UnGuardedRhs $ Do (bs ++ [retState])
        retState  = Qualifier (metaFunction "return" [recExp])
        recExp    = RecConstr (UnQual (name stateRecordCons)) [FieldWildcard]
        bs        = withStateFields ci concat absArb arbPC arbPtr arbVal arbInt arbGlobVal arbGlob
        bind v e  = Generator noLoc (pvar (name v)) e
        absArb _ b u pc = [ bind b arbPos
                          , bind u (arbRange (intE 0) (vExp b))
                          , bind pc arbEmptyMap
                          ]
        arbPC p v      = arbInt p v
        arbPtr p rd wr = concat [arbInt p rd, arbInt p wr]
        arbInt p v     = [bind v $ if isAbs p then arbEmptyMap else arbZero]
        arbVal p v     = [bind v $ if isAbs p then arbEmptyMap else arbNull]
        arbGlobVal v   = [bind v arbNull]
        arbGlob v      = [bind v arbZero]

-- instance Arbitrary p1 => Arbitrary (Pid_pre p1) where
--         arbitrary = oneof [return PIDR0, PIDR2 <$> arbitrary, ...]
arbitraryPidPreDecl    :: ConfigInfo Int -> Decl
arbitraryPidPreDecl ci =
  InstDecl noLoc Nothing [] ctx tc_name [tv_name] [InsDecl (FunBind [arb])]
  where
        -- possible context class
        ctx     = [ClassA tc_name [TyVar v] | v <- pid_vars]
        tc_name = UnQual $ name "Arbitrary"
        -- Pid_pre p1 ...
        tv_name = TyParen (foldl' TyApp (TyVar $ name pidPre) [TyVar v | v <- pid_vars])
        arb     = Match noLoc (name "arbitrary") [] Nothing arb_rhs Nothing
        arb_rhs = UnGuardedRhs (app (var' "oneof") (listE pid_ts))
        pid_ts  = Prelude.map pidGen ts

        pidName p = Con $ UnQual $ name (pidConstructor p)
        pidGen (p, t) = if isAbs p
                          then fmap_syn (pidName p) (var' "arbitrary")
                          else app (var' "return") (pidName p)

        var' s  = var $ name s
        pid_vars = [ t | (p, t) <- ts, isAbs p  ]
        ts       = [ (p, mkTy t) | p <- pids ci | t <- [0..] ]
        mkTy     = name . ("p" ++) . show

-- ### Aeson (FromJSON, ToJSON) instances ##################################################

-- instance FromJSON State where
--   parseJSON (Object s) = State <$>
--                          s .: "pidR0Pc" <*>
--                          ...
--                          s .: "x_3"
--   parseJSON _            = mzero
stateFromJSONDecl    :: ConfigInfo Int -> Decl
stateFromJSONDecl ci =
  InstDecl noLoc Nothing [] [] tc_name [tv_name] [InsDecl (FunBind [fr, fr_err])]
  where
        fr = Match noLoc (name "parseJSON") fr_arg Nothing (UnGuardedRhs rhs) Nothing
        -- do { var1 <- ..., ..., return State{...} }
        rhs = Do (bs ++ [Qualifier ret])
        ret = metaFunction "return" [RecConstr (qname stateRecordCons) [FieldWildcard]]
        -- accessor for each variable
        varParser n = Generator noLoc (pvarn n)
                      (infix_syn ".:" (var $ name "s") (Lit $ String n))
        -- parser for each variable
        bs = withStateFields ci concat absFs pcFs ptrFs field field glob glob
        absFs _ x y z = [varParser x, varParser y, varParser z]
        pcFs _ f      = [varParser f]
        ptrFs _ f1 f2 = [varParser f1, varParser f2]
        field _ f     = [varParser f]
        glob          = return . varParser


        -- parseJSON _ = mzero
        fr_err  = Match noLoc (name "parseJSON") [PWildCard] Nothing rhs_err Nothing
        rhs_err = UnGuardedRhs $ var $ name $ "mzero"

        tc_name = UnQual $ name "FromJSON"
        tv_name = TyVar $ name stateRecordCons
        qname n = UnQual $ name n
        pvarn n = Language.Haskell.Exts.Syntax.PVar $ name n
        fr_arg = [PApp (qname "Object") [pvarn "s"]]


-- instance ToJSON State where
--   toJSON State{..} = object [ "pidR0Pc"    .= pidR0Pc
--                             , ...
--                             , "x_3"        .= x_3        ]
stateToJSONDecl    :: ConfigInfo Int -> Decl
stateToJSONDecl ci =
  InstDecl noLoc Nothing [] [] tc_name [tv_name] [InsDecl (FunBind [to])]
  where
        to = Match noLoc (name "toJSON") to_arg Nothing (UnGuardedRhs rhs) Nothing

        -- = object ["<var 1>" .= <var 1>, ..., "<var n>" .= <var n>]
        rhs = app (var $ name "object") (listE exps)

        -- ["<var 1>" .= <var 1>, ..., "<var n>" .= <var n>]
        -- exps = map (\(n,_) -> infix_syn ".=" (Lit $ String n) (var $ name n))
        --            (stateVarsNTList ci)
        enc v = infix_syn ".=" (Lit $ String v) (var $ name v)
        exps = withStateFields ci concat absFs pcFs ptrFs field field glob glob
        absFs _ x y z = [enc x, enc y, enc z]
        pcFs _ f      = [enc f]
        ptrFs _ f1 f2 = [enc f1, enc f2]
        field _ f     = [enc f]
        glob          = return . enc

        -- parseJSON _ = mzero
        fr_err  = Match noLoc (name "parseJSON") [PWildCard] Nothing rhs_err Nothing
        rhs_err = UnGuardedRhs $ var $ name $ "mzero"
        tc_name = UnQual $ name "ToJSON"
        tv_name = TyVar $ name stateRecordCons
        qname n = UnQual $ name n
        pvarn n = Language.Haskell.Exts.Syntax.PVar $ name n
        -- State{..}
        to_arg = [PRec (qname "State") [PFieldWildcard]]

-- instance (FromJSON p1) => FromJSON (Pid_pre p1) where
--   parseJSON (Object o) = case toList o of
--     [(k,v)] | k == "PIDR0" -> return PIDR0
--             | k == "PIDR2" -> PIDR2 <$> parseJSON v
--   parseJSON _ = mzero
pidFromJSONDecl    :: ConfigInfo Int -> Decl
pidFromJSONDecl ci =
  InstDecl noLoc Nothing [] ctx tc_name [tv_name] [InsDecl (FunBind [fr, fr_err])]
  where
        fr     = Match noLoc (name "parseJSON") fr_arg Nothing (UnGuardedRhs rhs) Nothing
        fr_arg = [PApp (qname "Object") [pvarn "o"]]

        -- case H.toList o of ...
        rhs = Case (app (qvar (ModuleName "H") (name "toList")) (var' "o")) alts

        -- [(key,value)]
        alts     = [Alt noLoc case_pat case_rhs Nothing]

        -- [(k,v)] | ...
        case_pat = PList [PTuple Boxed [pvar' "k", pvar' "v"]]
        -- ... | k = PIDR0 -> return PIDR0 ...
        case_rhs = GuardedRhss guards

        guards   = [ GuardedRhs noLoc [g_stmt p] (g_exp p) | p <- pids ci ]
        g_stmt p = Qualifier $ infix_syn "==" (var' "k")
                                              (Lit $ String (pidConstructor p))
        g_exp p = let pidCon = Con $ UnQual $ name (pidConstructor p)
                  in if isAbs p
                     then fmap_syn pidCon (app (var' "parseJSON") (var' "v"))
                     else app (var' "return") pidCon

        -- parseJSON _ = mzero
        fr_err  = Match noLoc (name "parseJSON") [PWildCard] Nothing rhs_err Nothing
        rhs_err = UnGuardedRhs $ var $ name $ "mzero"

        -- possible context class
        ctx     = [ClassA tc_name [TyVar v] | v <- pid_vars]
        tc_name = UnQual $ name "FromJSON"
        -- Pid_pre p1 ...
        tv_name = TyParen (foldl' TyApp (TyVar $ name pidPre) [TyVar v | v <- pid_vars])

        var' s  = var $ name s
        pvar' s = pvar $ name s
        qname n = UnQual $ name n
        pvarn n = Language.Haskell.Exts.Syntax.PVar $ name n

        pid_vars = [ t | (p, t) <- ts, isAbs p  ]
        ts       = [ (p, mkTy t) | p <- pids ci | t <- [0..] ]
        mkTy     = name . ("p" ++) . show

-- instance (ToJSON p1) => ToJSON (Pid_pre p1) where
--   toJSON PIDR0       = object [ "PIDR0" .= Null ]
--   toJSON (PIDR2 pid) = object [ "PIDR2" .= toJSON pid ]
pidToJSONDecl    :: ConfigInfo Int -> Decl
pidToJSONDecl ci =
  InstDecl noLoc Nothing [] ctx tc_name [tv_name] [InsDecl (FunBind tos)]
  where
        tos = [Match noLoc
                     (name "toJSON")
                     [to_arg p]
                     Nothing
                     (UnGuardedRhs (to_rhs p))
                     Nothing | p <- pids ci]

        to_arg p = if isAbs p
                   then PParen $ PApp (qname (pidConstructor p)) [pvarn "pid"]
                   else PApp (qname (pidConstructor p)) []

        to_rhs p = app (var' "object")
                       (listE [infix_syn ".="
                                         (Lit $ String (pidConstructor p))
                                         (if isAbs p
                                             then app (var' "toJSON") (var' "pid")
                                             else Con $ qname "Null")])

        -- possible context class
        ctx     = [ClassA tc_name [TyVar v] | v <- pid_vars]
        tc_name = UnQual $ name "ToJSON"
        -- Pid_pre p1 ...
        tv_name = TyParen (foldl' TyApp (TyVar $ name pidPre) [TyVar v | v <- pid_vars])

        var' s  = var $ name s
        qname n = UnQual $ name n
        pvarn n = Language.Haskell.Exts.Syntax.PVar $ name n

        pid_vars = [ t | (p, t) <- ts, isAbs p  ]
        ts       = [ (p, mkTy t) | p <- pids ci | t <- [0..] ]
        mkTy     = name . ("p" ++) . show

-- ### "Static" functions to be included ##################################################
qcDefsStr="fn          = \"states.json\"\n\
\sample_size = 100000\n"

qcMainStr="main :: IO () \n\
\main = do b <- doesFileExist fn \n\
\          when b (removeFile fn) \n\
\\n\
\          inputs  <- generate $ vector sample_size :: IO [State] \n\
\          results <- mapM runTest inputs \n\
\          let results' = filter (not . null . fst) results \n\
\          C.writeFile fn (encodePretty $ toJSON results') \n\
\\n\
\          bs <- C.readFile fn \n\
\          let Just l = decode bs :: Maybe [([State],[Pid])] \n\
\          Prelude.putStrLn (\"Successfull runs: \" ++ (show $ length l)) \n"


arbValStr = "instance (Arbitrary a) => Arbitrary (Val a) where \n\
\  arbitrary = oneof [ return VUnit \n\
\                    , return VUnInit \n\
\                    , VInt    <$> arbitrary \n\
\                    , VString <$> arbitrary \n\
\                    , VPid    <$> arbitrary \n\
\                    , VInL    <$> arbitrary \n\
\                    , VInR    <$> arbitrary \n\
\                    , VPair   <$> arbitrary <*> arbitrary ]\n"

arbVecStr="instance (Arbitrary a) => Arbitrary (Vec a) where \n\
\   arbitrary = do a <- arbitrary \n\
\                  return $ mkVec a\n"
