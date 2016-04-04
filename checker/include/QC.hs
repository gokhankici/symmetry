{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
module Main where

import SymVector
import SymMap
import SymVerify
import SymBoilerPlate
import Test.QuickCheck
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Monad
import Data.ByteString.Lazy.Char8 as C (putStrLn, writeFile, appendFile, readFile)
import Data.Maybe
import Data.Either
import Data.String
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import System.Directory
import System.Exit

main :: IO ()
main = do b <- doesFileExist fn 
          when b (removeFile fn) 

          C.writeFile  fn (fromString "var stateCount=\n") 
          C.appendFile fn (encode size_obj) 
          C.appendFile fn (fromString "\n") 
          C.appendFile fn (fromString "var states =\n") 
          C.appendFile fn (fromString "[\n") 

          replicateM_ (sample_size-1) (printResult >> comma) 
          printResult 
          C.appendFile fn (fromString "]\n") 

          exitSuccess 

          where printResult = do result <- runTest 
                                 when (isLeft $ snd result) exitFailure 
                                 C.appendFile fn (encode $ toJSON result) 
                                 newLine 
                newLine = C.appendFile fn (fromString "\n") 
                comma   = C.appendFile fn (fromString ",") 



