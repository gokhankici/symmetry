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
import Test.QuickCheck.Monadic
import Data.List
import Data.Aeson
import Data.Aeson.Encode.Pretty
-- import Data.ByteString.Lazy.Char8 as C (putStrLn, writeFile, readFile)
-- import Data.HashMap.Strict as H hiding (map,filter,null)
import Data.Maybe
import Control.Monad
import System.Directory
import Debug.Trace

{-
    1. grammar
       (pc_i = PCI) ∧ ... => ...
    2. generate type from grammar
       ∀ s ∈ states. ∃ t ∈ types. s ∈ t
-}

data Grammar = Imp [Ant] [Con]
               deriving (Show, Eq)

data Ant     = AntPC Pid PCOp Int
               deriving (Show, Eq)

data PCOp    = PCEq
               deriving (Show, Eq)

data Con     = ConBool
               deriving (Show, Eq)

-- check        :: a -> SpecType -> Target (Bool, Expr)
-- type SpecType = RRType RReft
-- type RRType   = RType RTyCon RTyVar
-- type RReft    = UReft Reft

maxNoOfPids = 3

pcCount :: Pid -> Int
pcCount _ = trace "\n############## !!! implement pcCount !!! #################" 7

grammarPidGen n =
  do pids <- suchThat arbitrary pidPred >>= return . nub
     ops  <- vectorOf (length pids) (elements [PCEq])
     pcs  <- forM pids pcGen
     return $ (uncurry3 AntPC) <$> (zip3 pids ops pcs)

  where pidPred l  = 0 < length l' && length l' <= n
                     where l' = nub l
        pcGen pid  = oneof [ return (-1)
                           , elements [0..pcCount pid]]
        uncurry3 f (x,y,z) = f x y z

instance Arbitrary Grammar where
  arbitrary =
    Imp <$> (grammarPidGen maxNoOfPids) <*> con_gen
    where con_gen = return []

testTargetClient :: IO ()
testTargetClient  = do putStrLn $(testTH ''State)
                       g <- generate arbitrary :: IO Grammar
                       putStrLn (show g)
                       return ()
