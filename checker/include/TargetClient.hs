{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module TargetClient () where
import SymVector
import SymVector
import SymMap
import SymVerify
import SymBoilerPlate
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Monad
import Data.ByteString.Lazy.Char8 as C (putStrLn, writeFile, readFile)
import Data.HashMap.Strict as H hiding (map,filter,null)
import Data.Maybe
import System.Directory

{-
    1. grammar
       (pc_i = PCI) ∧ ... => ...
    2. generate type from grammar
       ∀ s ∈ states. ∃ t ∈ types. s ∈ t
-}

data Grammar = Imp [Ant] [Con]
data Ant     = AntPC Pid_pre Int
data Con     = ConBool

-- check        :: a -> SpecType -> Target (Bool, Expr)
-- type SpecType = RRType RReft
-- type RRType   = RType RTyCon RTyVar
-- type RReft    = UReft Reft
