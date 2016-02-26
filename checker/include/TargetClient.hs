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
           | AVal String     -- Val Int
           | APtr String Int -- Map_t Int Int
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

testTargetClient :: IO ()
testTargetClient  = do let [(s,ts)] = $(testTH ''State)
                       forM_ ts (putStrLn . show)
                       return ()
