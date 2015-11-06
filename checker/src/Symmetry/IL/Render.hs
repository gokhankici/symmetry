{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Symmetry.IL.Render where

import           Prelude hiding (concat, concatMap, sum, mapM, sequence, foldl, minimum, maximum, (<$>))
import           Text.PrettyPrint.Leijen
import           Control.Monad.Reader
import           Data.Function
import           Data.Foldable
import           Data.Maybe
import qualified Data.Map.Strict as M
import           Data.List (intercalate, groupBy, nub, partition)
import           Data.Generics hiding (empty)
import           Control.Exception
import           System.IO
import           Debug.Trace
import           Text.Printf
import           Data.List

import           Symmetry.IL.AST

debug msg x =
  trace (msg ++ show x) x

-- | Some useful combinators
byte :: Doc
byte = text "byte"

($$) :: Doc -> Doc -> Doc
x $$ y = align (x <$> y)

atomic :: [Doc] -> Doc
atomic ss
  = text "atomic" <+> block ss

block :: [Doc] -> Doc
block ds
  = lbrace <$> indent 2 (seqStmts ds) <$> rbrace

ifs :: [Doc] -> Doc
ifs []
  = int 0 <+> text "/* No Alternatives (perhaps nobody to receive from?) */"
ifs ss
  = text "if" <$> indent 2 (nonDetStmts ss) <$> text "fi"

doLoop :: [Doc] -> Doc
doLoop ss
  = text "do" <$> indent 2 (nonDetStmts ss) <$> text "od"

forLoop :: Doc -> Doc -> Doc
forLoop i body
  = text "for" <+> parens i <+> lbrace <$>
                   indent 2 body <$>
                   rbrace

nonDetStmts :: [Doc] -> Doc
nonDetStmts ss
  = vcat $ map (text "::" <+>) ss

seqStmts :: [Doc] -> Doc
seqStmts ss = vcat . punctuate semi $ ss

-- | Rendering PIDs
renderProcPrefix :: Pid -> Doc
renderProcPrefix (PConc _)         = text "Conc_"
renderProcPrefix (PAbs _ (S s))    = text s
renderProcPrefix (PUnfold _ (S s) _) = text s
renderProcPrefix (PVar _)          = empty

renderProcName :: Pid -> Doc
renderProcName p@(PConc i)         = renderProcPrefix p <> int i
renderProcName p@(PAbs (V v) _)    = renderProcPrefix p <> brackets (text v)
renderProcName p@(PUnfold (V v) _ _) = renderProcPrefix p <> brackets (text v)
renderProcName p@(PVar (V v))      = renderProcPrefix p <> text v

renderProcTypeName :: Pid -> Doc
renderProcTypeName   (PVar _)     = error "renderProcTypeName Var"
renderProcTypeName p@(PConc _)    = renderProcName p <> text "_Process"
renderProcTypeName p@(PAbs _ _)   = renderProcPrefix p <> text "_Process"
renderProcTypeName p@(PUnfold {}) = renderProcPrefix p <> text "_Process_Unfold"

renderProcTypeArgs :: Pid -> Doc
renderProcTypeArgs (PVar _)            = error "renderProcTypeArgs Var"
renderProcTypeArgs (PConc _)           = parens empty
renderProcTypeArgs (PUnfold (V v) _ _) = parens (byte <+> text v)
renderProcTypeArgs (PAbs (V v) _)      = parens (byte <+> text v)

evalExp :: Doc -> Doc
evalExp e = text "eval" <> parens e

renderEvalPids  :: MConstr -> [Doc]
renderEvalPids
  = map go . tyargs
  where
    go p@(PVar _) = renderProcName p
    go p@(PUnfold _ _ i) = evalExp (renderProcPrefix p <> brackets (int i))
    go p = evalExp (renderProcName p)

renderPids  :: MConstr -> [Doc]
renderPids
  = map renderProcName . tyargs

-- | Messages
renderMConstr :: MConstr -> Doc
renderMConstr MTApp{ tycon = tc }
  = text $ untycon tc
renderMConstr (MCaseL _ m)
  = renderMConstr m
renderMConstr (MCaseR _ m)
  = renderMConstr m
renderMConstr (MTProd m1 m2)
  = renderMConstr m1 <> renderMConstr m2

inlCstr = text "LL"
inrCstr = text "RR"

renderSendMsg :: MConstr -> Doc
renderSendMsg = hcat . punctuate comma . go
  where
    go m@(MTApp _ [])
      = [renderMConstr m]
    go m@(MTApp _ _)
      = renderMConstr m : (punctuate comma $ renderPids m)
    go (MCaseL l c) = renderLabel l : go c
    go (MCaseR l c) = renderLabel l : go c
    go (MTProd c d) = go c ++ go d

renderLabel LL         = inlCstr
renderLabel RL         = inrCstr
renderLabel (VL (V x)) = text x

renderRecvMsg :: MConstr -> Doc
renderRecvMsg = hcat . punctuate comma . go
  where
    go m@(MTApp _ [])
      = [renderMConstr m]
    go m@(MTApp _ _)
      = renderMConstr m : (punctuate comma $ renderEvalPids m)
    go (MCaseL l c) = renderLabel l : go c
    go (MCaseR l c) = renderLabel l : go c
    go (MTProd c d) = go c ++ go d

builtInCstr :: [Doc]
builtInCstr = [ inlCstr, inrCstr ]

renderMConstrs :: MTypeEnv -> Doc
renderMConstrs ts
  = text "mtype"
    <+> equals
    <+> encloseSep lbrace rbrace comma (rts ++ builtInCstr)
    <> semi
  where
    rts = map renderMConstr . nub $ listify f ts
    f (MTApp{}) = True
    f _         = False

chanTy :: Doc
chanTy = text "chan"

switchBase :: TId -> CId -> Doc
switchBase t c = text "tcs_" <> int t <> text "_" <> int c

-- | Addressing Process Channels
switchboard :: Int -> Pid -> Pid -> (TId, CId) -> Doc
switchboard np from p (t,c) =
  switchBase t c <+> brackets (
                          (renderProcVar from <> text "*" <> int np)
                          <+> text "+"
                          <+> renderProcVar p
                     )

initSwitchboard :: Int -> Pid -> Pid -> (TId, CId) -> Doc
initSwitchboard np p q (t, c)
  = switchboard np p q (t, c) <+>
    equals <+>
    renderChanName p q t c

-- initSwitchboard n p@(PAbs (V v) (S _)) t
--   = forLoop (text v <+> text "in" <+> renderProcPrefix p) $
--     (switchboard n p t <+> equals <+> renderChanName p q t <> brackets (text v))

renderChanName :: Pid -> Pid -> TId -> CId -> Doc
renderChanName p q ti cj
  =  renderProcChanName p <> text "_" <>
     renderProcChanName q <> text "_" <>
     text "t" <> int ti <> text "c" <> int cj
     -- renderMConstr t
       where
         renderProcChanName (PVar _)
           = error "renderChanName var"
         renderProcChanName pid@(PConc _)
           = renderProcName pid
         renderProcChanName pid@(PAbs _ _)
           = renderProcPrefix pid
         renderProcChanName pid@(PUnfold (V v) (S s) i)
           = renderProcPrefix pid <> text "_Unfold" <> int i

renderProcDecl :: Doc -> Doc
renderProcDecl n
  = byte <+> n

renderProcAssn :: Doc -> Doc -> Doc
renderProcAssn n rhs
  = renderProcDecl n <+> equals <+> rhs

type SetMap  = M.Map Set Int
type ChanMap = Pid -> Pid -> (TId, CId) -> Doc
type StmtMap = M.Map Int [Int]
type ProcMap = M.Map Int Pid

-- | Emit Promela code for each statement
data RenderEnv = RE { chanMap :: ChanMap
                    , pidMap  :: PidMap
                    , setMap  :: SetMap
                    , stmtMap :: StmtMap
                    , procMap :: ProcMap
                    }

type RenderM = Reader RenderEnv Doc

renderStmt :: Pid -> Stmt Int -> RenderM
renderStmt (p @ (PConc _)) s    = do r <- renderStmtConc p s
                                     l <- line_directive s
                                     return $ l <$> r
renderStmt (p @ (PUnfold {})) s = do r <- renderStmtConc p s
                                     l <- line_directive s
                                     return $ l <$> r
renderStmt p s                  = do r <- renderStmtAbs p s
                                     l <- line_directive s
                                     return $ l <$> r

-- renderStmt f p s               = nonDetStmts $ map (renderStmtAbs f p) ss
--   where
--     ss = listify (\(_::Stmt Int) -> True) s

pid_short    :: Pid -> String
pid_short pid =
  case pid of
    PConc i               -> show i
    PAbs (V v) (S s)      -> "abs " ++ v ++ " " ++ s
    PUnfold (V v) (S s) i -> "unf " ++ v ++ " " ++ s ++ " " ++ (show i)
    PVar (V v)            -> "pvar " ++ v

line_directive  :: Stmt Int -> RenderM
line_directive s =
  do pm <- asks procMap
     let sid      = annot s
         procId_s = case M.lookup sid pm of
                      Nothing  -> error "missing (sid,pid) pair"
                      Just pid -> pid_short pid
     return $ text $ printf "#line %d \"%s:%d\"" (1 :: Int) procId_s sid

sendMsg :: Pid -> Pid -> (TId, CId, MConstr) -> Int -> RenderM
sendMsg p q (t,c,v) i
  = do f <- asks chanMap
       return $ f p q (t,c) <> text "!" <> renderSendMsg v

pollMsg :: Pid -> Pid -> (TId, CId, MConstr) -> RenderM
pollMsg p q (t,c,m)
  = do f <- asks chanMap
       return $ f p q (t,c) <> text "?" <> brackets (renderRecvMsg m)

recvMsg :: Pid -> Pid -> (TId, CId, MConstr) -> RenderM
recvMsg p q (t,c,m)
  = do f <- asks chanMap
       return $ text "__RECV" <> tupled [f p q (t,c) , renderRecvMsg m]

renderStmtConc :: Pid -> Stmt Int -> RenderM
renderStmtConc me (SSend p m i)
  = sendMsg me p m i

renderStmtConc me (SRecv m _)
  = do ps <- fmap (filter (/= me) . M.keys) (asks pidMap)
       rs <- mapM (go m . subUnfoldIdx) ps
       return $ ifs rs
  where
    go m p = recvMsg p me m

renderStmtConc me (SCase l sl sr _)
  = do dl <- renderStmtConc me sl
       ll <- line_directive sl
       dr <- renderStmtConc me sr
       lr <- line_directive sr
       return $ ifs [ ll <$> isLeftLabel l dl
                    , lr <$> isRightLabel l dr
                    ]

renderStmtConc _ (SSkip _)
  = return $ text "skip"

renderStmtConc me (SIter (V v) (S s) ss _)
  = do d <- renderStmt me ss
       return $ forLoop (text ("_it_" ++ v) <+> text "in" <+> text s)
                        (text v <+> equals <+> renderProcName (PAbs (V ("_it_" ++ v)) (S s)) <> semi <$> d)
renderStmtConc me (SLoop (LV v) ss _)
  = do d <- renderStmt me ss
       return $ int 1 <> semi <+> text v <> text ":" <+> block [d]

renderStmtConc _ (SVar (LV v) _)
  = return $ text "goto" <+> text v

renderStmtConc me (SChoose (V v) s ss _)
  = do d <- renderStmt me ss
       sm <- asks setMap
       return $ block [ text "select" <+>
                        parens (text v <+> text ":" <+> int 0 <+> text ".." <+> sz sm)
                      , renderProcName (PVar (V v)) <+> equals <+> renderProcName (PAbs (V v) s)
                      , d
                      ]
  where
    sz sm  = maybe err int $ M.lookup s sm
    err = error ("Unknown set (renderStmtConc): " ++ show s)

renderStmtConc me (SBlock ss _)
  = fmap seqStmts $ mapM (renderStmt me) ss

renderStmtConc me (SNonDet ss _)
  = fmap ifs $ mapM (renderStmt me) ss

renderStmtConc _ s@(SDie _)
  = return $ text "assert(0 == 1) /* CRASH! */"

renderStmtConc _ (SCompare (V v) p1 p2 _)
  = return $ text v <+> equals <+>
    parens (renderProcVar p1 <+> eq <+> renderProcVar p2 <+>
                          text "->" <+> inlCstr <+> colon <+> inrCstr)


renderStmtConc _ _
  = return $ text "assert(0 == 1) /* TBD */"

eq = equals <> equals
isLeftLabel (V x) s  = align (text x <+> eq <+> inlCstr <+> text "->" <$> s)
isRightLabel (V x) s = align (text x <+> eq <+> inrCstr <+> text "->" <$> s)

stmtLabel :: Int -> Doc
stmtLabel i =
  text "stmt_L" <> int i

label :: Doc -> Doc -> Doc
label l d =
  l <> text ":" <+> d

labelStmt :: Int -> Doc -> Doc
labelStmt i s
  = label (stmtLabel i) s

inOutCounters :: M.Map Int [Int]
              -> Int
              -> [Doc]
inOutCounters cfg i
  = case outCounters cfg i of
      []  -> [decrCounter i]
      ocs -> decrCounter i : ocs

outCounters :: M.Map Int [Int]
              -> Int
              -> [Doc]
outCounters cfg i
  = case M.lookup i cfg of
      Nothing -> []
      Just js -> map incrCounter js

renderStmtAbs :: Pid
              -> Stmt Int
              -> RenderM
renderStmtAbs me s@(SSend _ _ i)
  = do send <- renderStmtConc me s
       cfg <- asks stmtMap
       return $ block (send : inOutCounters cfg i)

renderStmtAbs me s@(SRecv _ i)
  = do cfg       <- asks stmtMap
       recv      <- renderStmtConc me s
       return $ ifs [ seqStmts (recv : inOutCounters cfg i)
                    , text "timeout"
                    ]
  --      let outms  = assert (length ms == length outcs) $ zip nullms outcs
  --          outcs  = outCounters cfg i
  --          nullms =  [(t, cs, SNull) | (t, cs, _) <- ms]
  --      rs        <- mapM render1 outms
  --      return $ ifs rs
  -- where
  --   render1 (m, oc) = do d <- renderStmtConc me $ SRecv [m] i
  --                        return $ block [d, decrCounter i, oc]

renderStmtAbs _ (SCase v l r i)
  = return $ ifs [ isLeftLabel v  $ seqStmts [decrCounter i, incrCounter (annot l)]
                 , isRightLabel v $ seqStmts [decrCounter i, incrCounter (annot r)]
                 ]

renderStmtAbs me (SNonDet ss i)
  = do cfg  <- asks stmtMap
       let outcs = outCounters cfg i
           grds  = zip outcs $ map (recvGuard me) ss
       rs <- mapM go grds
       return $ ifs rs
  where
    go (oc, Just g) = do grd <- g
                         return (grd <+> text "->" <+> block [decrCounter i, oc])
    go (oc, Nothing) = return (block [decrCounter i, oc])

renderStmtAbs _ (SVar _ i)
  = inOutCountersM i

renderStmtAbs _ (SSkip i)
  = inOutCountersM i

renderStmtAbs _ (SLoop _ _ i)
  = inOutCountersM i

renderStmtAbs _ (SNull {})
  = return $ text "skip"

renderStmtAbs _ (SDie _)
  = return $ text "assert (0 == 1) /* CRASH! */"

renderStmtAbs _ (SBlock _ i)
  = inOutCountersM i

renderStmtAbs _ s
  = return . text $ "assert(0 == 1) /* TBD:" ++ show s ++ "*/"

inOutCountersM :: Int -> RenderM
inOutCountersM i = do cfg <- asks stmtMap
                      return $ block (inOutCounters cfg i)

nonDetSkip :: Int -> Doc
nonDetSkip i
  = labelStmt i $
    ifs [ stmtCounter i <+> text ">" <+> int 0 <+> text "->" <+> text "skip"
        , gotoStmt (i + 1)
        ]

stmtCounter :: Int -> Doc
stmtCounter i
  = text "k_" <> int i

decrCounter :: Int -> Doc
decrCounter i
  = text "__DEC" <> parens (stmtCounter i)

incrCounter :: Int -> Doc
incrCounter i
  = text "__INC" <> parens (stmtCounter i)

gotoStmt :: Int -> Doc
gotoStmt i
  = text "goto" <+> stmtLabel i

chanGuard :: Pid -> (TId, CId, MConstr) -> RenderM
chanGuard me (t,c,v)
  = do pm <- asks pidMap
       fmap (vcat . punctuate (text " || ")) .
         mapM go $ M.keys pm
  where
    go them = pollMsg them me (t,c,v)

recvGuard :: Pid -> Stmt Int -> Maybe RenderM
recvGuard me (SNonDet ss _)
  = do let gs = mapMaybe (recvGuard me) ss
       case gs of
         [] -> Nothing
         _  -> Just $ do
                 fmap (vcat . punctuate (text " || ")) (sequence gs)

recvGuard me (SBlock (s:_) _)
  = recvGuard me s

recvGuard me (SRecv m _)
  = Just $ chanGuard me m

recvGuard _ _
  = Nothing

stmtGuard :: Pid -> Stmt Int -> RenderM
stmtGuard me s
  = case rg of
      Just g -> do d <- g
                   return $ (align (base <+> text "&&" <+> parens d))
      _      -> return base
  where
    rg   = recvGuard me s
    base = stmtCounter (annot s) <+> text ">" <+> int 0

-- stmtGuard _ s
--   = return $ stmtCounter (annot s) <+> text ">" <+> int 0

selectStmt :: Pid -> [Stmt Int] -> RenderM
selectStmt me ss
  = do ds <- mapM goto ss
       return . ifs $ exit : ds
    where
      exit = hcat . punctuate (text " && ") $ map (<+> text "== 0") ks
      ks   = map (stmtCounter .  annot) ss
      goto :: Stmt Int -> RenderM
      goto s = do sg <- stmtGuard me s
                  return $ seqStmts [sg, gotoStmt $ annot s]

(<?$>) :: [Doc] -> Doc -> Doc
[] <?$> y = y
xs <?$> y = vcat xs <$> y

renderProcStmts :: Process Int -> RenderM
renderProcStmts (p, ss)
  | isAbs p
    = do ds <- mapM do1Abs absStatements
         seld <- selectStmt p $ listify (const True) ss
         return $ [ byte <+> text v <> semi | (V v) <- recvVars vs ] <?$>
                  ([ byte <+> text ("_it_" ++ v) <> semi | v <- nub ivs ] <?$>)
                  (label (text "end") . labelStmt 0 $ seld) <$>
                  (text "goto exit" <> semi) <$>
                  (seqStmts ds) <$>
                  (label (text "exit") empty)
  | otherwise
    = do ds <- renderStmt p ss
         return $ [ byte <+> text v <> semi | (V v) <- recvVars vs ] <?$>
                  ([ byte <+> text ("_it_" ++ v) <> semi | v <- nub ivs ] <?$>
                   ds <$> label (text "end") (text "0"))
  where
    do1Abs s = do d <- renderStmt p s
                  return . labelStmt (annot s)
                         $ atomic [d, gotoStmt 0]

    absStatements = listify absOK ss

    absOK (SNull {})   = False
    absOK _            = True
    recvVars            = filter (not . isProcVar p) . nub
    isProcVar (PAbs v _) v'    = v == v'
    isProcVar (PUnfold v _ _) v' = v == v'
    isProcVar _          _  = False
    vs                      = listify go ss :: [Var]
    ivs                     = everything (++) (mkQ [] iterVar) ss
    iterVar :: Stmt Int -> [String]
    iterVar (SIter (V v) _ _ _) = [v]
    iterVar _                   = []
    go :: Var -> Bool
    go                      = const True

renderProc :: Process Int -> RenderM
renderProc p
  = do ds <- renderProcStmts p
       return $ text "proctype" <+>
                renderProcTypeName (fst p) <+>
                renderProcTypeArgs (fst p) <$>
                if isAbs (fst p) then
                  block [ comments, ds ]
                else
                  block [ ds ]
  where
    comments = text "/* Counters for" <+>
               renderProcTypeName (fst p) <+>
               text "*/" <$>
               cvs
    cs   = foldl' (flip (:)) [] (snd p)
    cvs  = if isAbs (fst p) then
             byte <+>
               stmtCounter (head cs) <+>
               equals <+>
               text "__K__" <> semi <$>
             seqStmts [ byte <+>
                        stmtCounter c
                      | c <- nub $ tail cs ]
           else
             empty

renderProcs :: Config Int -> PidMap -> SetMap -> ProcMap -> Doc
renderProcs (Config { cTypes = ts, cProcs = ps }) pm sm procm
  = runReader (foldM render1 empty psfilter) env
  where
    -- For each unfolded process, it suffices to
    -- render the body once
    render1 d p = do pd <- renderProc p
                     return $ d <$$> pd
    psfilter = map head $ groupBy (eqUnfolds `on` fst) ps
    eqUnfolds (PUnfold v s _) (PUnfold v' s' _) = v == v' && s == s'
    eqUnfolds p1 p2 = p1 == p2
    env = RE { pidMap = pm
             , setMap = sm
             , chanMap = switchboard (length ps)
             , stmtMap = cfg
             , procMap = procm
             }
    cfg = M.unions (map (nextStmts . snd) psfilter)

renderMain :: PidMap -> Config a -> Doc
renderMain m (Config { cTypes = ts, cProcs = ps, cSets = bs })
  = text "init" <+> block body
  where
    body        = declAbsVars ++
                  [ atomic pidInits
                  , atomic boardInits
                  , atomic procInits
                  ]
    declAbsVars = [ byte <+> text v | PAbs (V v) _ <- map fst ps ]
    boardInits  = do let pids = map fst ps
                     p        <- pids
                     q        <- pids
                     (t,cm)   <- M.toList ts
                     (c,m)    <- M.toList cm
                     -- t        <- ts
                     return $ initSwitchboard (length ps) (subUnfoldIdx p) (subUnfoldIdx q) (t,c)
    procInits   = map (runProc m . fst) ps
    pidInits    = map ((\p -> assignProcVar bs p (sz p)) . fst) ps
    sz p        = fromMaybe (err p) $ M.lookup p m
    err p       = error ("Unknown pid (renderMain): " ++ show p)

runProc :: PidMap -> Pid -> Doc
runProc _ p@(PConc _)
  = text "run" <+>
    renderProcTypeName p <>
    parens empty

runProc _ p@(PAbs _ _)
  = text "run" <+> renderProcTypeName p <> parens (int 0)
  where

runProc _ p@(PUnfold _ _ i)
  = text "run" <+> renderProcTypeName p <> parens (int i)

renderProcVar :: Pid -> Doc
renderProcVar (PAbs _ (S s))
  = text s <> brackets (int 0)
renderProcVar (PUnfold (V v) (S s) _)
  = text s <> brackets (text v)
renderProcVar p
  = renderProcName p

assignProcVar :: [SetBound] -> Pid -> (Int, Int) -> Doc
assignProcVar _ p@(PConc _) (i, _)
  = renderProcVar p <+> equals <+> int i
assignProcVar _ (PAbs _ (S s)) (i, sz)
  = seqStmts $ map (uncurry go) (zip [0..setSize-1] (repeat i) ++
                                 zip [setSize..sz-1] [(i+1)..(i+sz-setSize)])
  where
    go j k = text s <> brackets (int j) <+> equals <+> int k
assignProcVar _ p@(PUnfold {}) (v, _)
  = unfoldProcVar p <+> equals <+> pid <$>
    renderProcVar (subUnfoldIdx p) <+> equals <+> unfoldProcVar p
      where
        pid = int v
assignProcVar _ _ _ = error "Attempt to assign to invalid ProcVar"


unfoldProcVar :: Pid -> Doc
unfoldProcVar p@(PUnfold _ _ i)
  = renderProcPrefix p <> text "Unfold_" <> int i

declProcVar :: Pid -> (Int, Int) ->  Doc
declProcVar p@(PConc _) _
  = renderProcDecl (renderProcName p) <> semi
declProcVar p@(PAbs (V _) (S _)) (_, sz)
  = renderProcDecl (renderProcPrefix  p) <> brackets (int sz) <> semi
declProcVar p@(PUnfold (V _) (S _) _) _
  = renderProcDecl (unfoldProcVar p) <> semi

declProcVars :: PidMap -> Doc
declProcVars
  = M.foldrWithKey go empty
  where
    go pid k d = declProcVar pid k <$> d

declSets :: [SetBound] -> Doc
declSets
  = foldr go empty
  where
    go (Bounded (S s) n) d = renderProcDecl (text s <> brackets (int n)) <> semi <$> d

chanMsgType :: MConstr -> Doc
chanMsgType
  = enclose lbrace rbrace . hcat . punctuate comma . chanMsgType'

chanMsgType' :: MConstr -> [Doc]
chanMsgType' (MTApp _ as)
  = text "mtype" : map (const byte) as
chanMsgType' (MCaseL _ c)
  = text "mtype" : chanMsgType' c
chanMsgType' (MCaseR _ c)
  = text "mtype" : chanMsgType' c
chanMsgType' (MTProd c c')
  = chanMsgType' c ++ chanMsgType' c'

declChannelsOfCstr :: PidMap -> (TId, CId, MConstr) -> Doc
declChannelsOfCstr pm (tid,cid,c) = vcat $ map go (prod $ M.keys pm)
  where
    prod ps = [ (p,q) | p <- ps, q <- ps ]
    go (p,q)
      = chanTy <+> renderChanName p q tid cid
               <+> equals
               <+> text "[__K__] of"
               <+> chanMsgType c
               <> semi

declChannelsOfType :: PidMap -> TId -> M.Map CId MConstr -> Doc
declChannelsOfType pm tid m
  = vcat $ M.foldrWithKey go [] m
  where
    go cid c docs = declChannelsOfCstr pm (tid, cid, c) : docs

declChannels :: PidMap -> MTypeEnv -> Doc
declChannels pm te
  = vcat $ M.foldrWithKey go [] te
  where
    go tid m docs = declChannelsOfType pm tid m : docs

buildProcMap :: Config Int -> ProcMap
buildProcMap (Config { cProcs = ps }) =
  let pairs = [ (sid,pid) | (pid,s) <- ps,
                            sid <- (foldl' (\acc i -> i:acc) [] s) ]
   in M.fromList pairs


buildPidMap :: Config a -> PidMap
buildPidMap (Config { cUnfold = us, cProcs = ps, cSets = bs})
  = fst $ foldl' go (foldl' go (M.empty, 0) (snd pids)) (fst pids)
  where
    pids                      = partition isUnfold $ map fst ps
    go (m, i) p@(PConc _)     = (M.insert p (i, 1) m, succ i)
    go (m, i) p@(PAbs _ set)  = let s = unfolds set + setSize
                                in (M.insert p (i, s) m, i + s)
    go (m, i) p@(PUnfold _ s _) = if isBound s bs then
                                    (M.insert p (i, 1) m, succ i)
                                  else
                                    updUnfold (m, i) p
    unfolds s                 = sum [ x | Conc s' x <- us, s == s' ]


updUnfold :: (PidMap, Int) -> Pid -> (PidMap, Int)
updUnfold (m, i) p@(PUnfold v s _)
  = (M.insertWith upd p (j, 1) m, i)
    where
      upd (a,b) (_,d) = (a, b+d)
      j               = maybe err (succ . fst) $ M.lookup (PAbs v s) m
      err             = error ("Unknown pid (updUnfold): " ++ show p)

-- renderSwitchboard :: Int -> Int -> Doc
-- renderSwitchboard nprocs ntypes
--   = chanTy <+> switchBase <+>
--     brackets (int (nprocs*nprocs*ntypes)) <> semi

declSwitchboardsCstr :: Int -> TId -> CId -> Doc
declSwitchboardsCstr nprocs tid cid
  = chanTy <+> switchBase tid cid
           <+> brackets (int (nprocs * nprocs)) <> semi

declSwitchboardsType :: Int -> TId -> M.Map CId MConstr -> Doc
declSwitchboardsType nprocs tid m
  = vcat $ map go $ M.keys m
  where
    go cid = declSwitchboardsCstr nprocs tid cid

declSwitchboards :: PidMap -> MTypeEnv -> Doc
declSwitchboards pm te
  = vcat $ M.foldrWithKey go [] te
  where
    go tid m docs = declSwitchboardsType nprocs tid m : docs
    nprocs = M.foldrWithKey (\p v s -> s + count p v) 0 pm
    count (PAbs _ _) (_, n) = n - setSize + 1
    count (PConc _) (_,n) = n
    count p@(PUnfold v s _) (_,n) =
      maybe n (const 0) $ M.lookup (PAbs v s) pm
    count _ _          = 0

macrify = intercalate " \\\n"
decMacro = macrify [ "#define __DEC(_x) atomic { assert (_x > 0);"
                   , "if"
                   , ":: (_x < __K__) ->  _x = _x - 1"
                   , ":: else; {"
                   , "if"
                   , ":: _x = _x - 1"
                   , ":: skip"
                   , "fi }"
                   , "fi }"
                   ]
recvMacro =
  macrify [ "#define __RECV(_i,...) atomic {"
          , "_i?[__VA_ARGS__];"
          , "if"
          , "  :: len(_i) < __K__ ->"
          , "     _i?__VA_ARGS__"
          , "  :: else ->"
          , "  if"
          , "  :: _i?__VA_ARGS__"
          , "  :: _i?<__VA_ARGS__>"
          , "  fi"
          , "fi }"
          ]

macros :: [Doc]
macros =
  [ text "#define __K__" <+> int infty
  , text "#define __INC(_x) _x = (_x < __K__ -> _x + 1 : _x)"
  , text decMacro
  , text recvMacro
  ]

render :: (Eq a, Show a) => Config a -> Doc
render c@(Config { cTypes = ts, cSets = bs })
  = all_stmts (cProcs unfolded) <$$> mtype
    <$$> align (vcat macros)
    <$$> declProcVars pMap <> declSets bs
    <$$> declChannels pMap ts
    <$$> declSwitchboards pMap ts
    <$$> procs
    <$$> text "#line 1 \"main\""
    <$$> renderMain pMap unfolded
  where
    unfolded               = filterBoundedAbs . freshIds . instAbs $ unfold c
    filterBoundedAbs c     = c { cProcs = [ p | p <- cProcs c, not (isBounded bs (fst p)) ] }
    pMap                   = buildPidMap unfolded
    procMap                = buildProcMap unfolded
    mtype                  = renderMConstrs ts
    procs                  = renderProcs unfolded pMap sMap procMap
    sMap                   = M.foldrWithKey goKey M.empty pMap
    goKey (PAbs _ s) (_,n) m = M.insert s n m
    goKey _          _     m = m

renderToFile :: (Eq a, Show a) => FilePath -> Config a -> IO ()
renderToFile fn c
  = withFile fn WriteMode $ flip hPutDoc (render c)

pid_arrow s  = int (annot s) <+> text "->"
short_stmt s = pid_arrow s <+> pretty_short s

stmt_to_str              :: Stmt Int -> Doc
stmt_to_str (SBlock ss a) = vcat $ text (printf "%d -> block %s" a (show $ map annot ss)) :
                                   map stmt_to_str ss

stmt_to_str s@(SIter v set body a) = vcat [short_stmt s, stmt_to_str body]

stmt_to_str s@(SLoop lv body a) = vcat [short_stmt s, stmt_to_str body]

stmt_to_str (SChoose v set body a) = vcat [ text $ printf "%d -> choose %d" a (annot body)
                                          , stmt_to_str body
                                          ]

stmt_to_str s@(SCase v sl sr a) = vcat [short_stmt s, stmt_to_str sl, stmt_to_str sr]

stmt_to_str (SNonDet ss a) = vcat $ (text $ printf "%d -> ND %s" a $ show $ map annot ss):
                                    map stmt_to_str ss

stmt_to_str s = int (annot s) <+> text "->" <+> pretty s

pretty_short (SIter x xs s a) =
  text "for" <+> parens (pretty x <+> colon <+> pretty xs) <+> braces (int $ annot s)

pretty_short (SLoop (LV v) s i) = pretty v <> colon <+> (int $ annot s)

pretty_short (SCase l sl sr _) =
  text "match" <+> pretty l <+> text "with" <$>
  indent 2
    (align (vcat [text "| InL ->" <+> (int $ annot sl),
                  text "| InR ->" <+> (int $ annot sr)]))


process_to_str (pid,s) =
  text (pid_short pid) <+> text "=>" <+> int (annot s) <$> stmt_to_str s

all_stmts :: [Process Int] -> Doc
all_stmts procs = text "/*" <$>
                    (vcat $ intersperse line $ map process_to_str procs) <$>
                    text "*/"
