{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module SymBoilerPlate where

import SymMap
import Control.Monad
import Data.Aeson
import Data.HashMap.Strict as H

import System.IO.Unsafe
import System.Random
import GHC.Generics
import Test.QuickCheck

{-@ nonDet :: a -> x:Int -> {v:Int | 0 <= v && v < x } @-}
nonDet :: a -> Int -> Int
nonDet _ x = nonDetRange 0 x 

{-@ nonDetRange :: x:Int -> y:Int -> {v:Int | x <= v && v < y} @-}  
nonDetRange :: Int -> Int -> Int
nonDetRange x y = unsafePerformIO $ do g      <- getStdGen
                                       (x, _) <- return $ randomR (x, y-1) g
                                       return x
                                        

instance DefaultMap Int where
  def = 0

instance DefaultMap (Val p) where
  def = VUnInit

{-@
 data Val p = VUnit {}
           | VUnInit {}
           | VInt { vInt :: Int }
           | VString { vString :: String }
           | VSet { vSetName :: String }
           | VPid { vPid :: p }
           | VInR { vInR :: Val p }
           | VInL { vInL :: Val p }
           | VPair { vLeft :: Val p, vRight :: Val p }
@-}
data Val p = VUnit {}
             | VUnInit {}
             | VInt { vInt :: Int }
             | VString { vString :: String }
             | VSet { vSetName :: String }
             | VPid { vPid :: p }
             | VInR { vInR :: Val p }
             | VInL { vInL :: Val p }
             | VPair { vLeft :: Val p, vRight :: Val p }
               deriving (Show, Eq, Ord, Generic)

instance (FromJSON p) => FromJSON (Val p)
instance (ToJSON p) => ToJSON (Val p)

instance (Arbitrary a) => Arbitrary (Val a) where
  arbitrary = oneof [ return VUnit
                    , return VUnInit
                    , VInt    <$> suchThat arbitrary (\ k -> (&&) ((<) 0 k) ((>) 5 k))
                    , VString <$> arbitrary
                    , VPid    <$> arbitrary
                    , VInL    <$> arbitrary
                    , VInR    <$> arbitrary
                    , VPair   <$> arbitrary <*> arbitrary ]


liquidAssert p x = if p
                     then Right x
                     else Left x

isVUnit, isVUnInit, isVInt, isVString, isVPid, isVInR, isVInL, isVPair, isVSet :: Val p -> Bool
isVUnit VUnit{} = True
isVUnit _       = False

isVUnInit VUnInit{} = True
isVUnInit _         = False

isVInt VInt{} = True
isVInt _      = False

isVString VString{} = True
isVString _         = False

isVSet VSet{} = True
isVSet _      = False

isVPid VPid{} = True
isVPid _         = False

isVInR VInR{} = True
isVInR _         = False

isVInL VInL{} = True
isVInL _         = False

isVPair VPair{} = True
isVPair _         = False

{-@ measure isVUnit   @-}
{-@ measure isVUnInit @-}
{-@ measure isVInt    @-}
{-@ measure isVString @-}
{-@ measure isVPid    @-}
{-@ measure isVInL    @-}
{-@ measure isVInR    @-}
{-@ measure isVPair   @-}
