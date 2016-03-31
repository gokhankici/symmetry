{-# LANGUAGE DeriveGeneric #-}
module SymMap where

import Data.HashMap.Strict as HM
import Data.Aeson
import Control.Monad as M
import Data.Vector as V
import GHC.Generics

data Map_t k v = M (HM.HashMap k v)
                 deriving (Show, Eq, Generic)

{-@ embed Map_t as Map_t @-}
{-@ measure Map_select :: Map_t k v -> k -> v @-}
{-@ measure Map_store :: Map_t k v -> k -> v -> Map_t k v @-}
 
{-@ get :: m:Map_t k v -> k:k -> {v:v | v = Map_select m k} @-}
get :: (Ord k, DefaultMap v) => Map_t k v -> k -> v
get (M m) k = HM.lookupDefault def k m

class DefaultMap v where
  def :: v

{-@ put :: m:Map_t k v -> k:k -> v:v -> {vv:Map_t k v | vv = Map_store m k v } @-}
put :: (Ord k) => Map_t k v -> k -> v -> Map_t k v
put (M m) k v = M (HM.insert k v m)

emptyMap = M (HM.empty)

singleton k v = M (HM.fromList [(k, v)])

-- export / import as list of (key,value)
instance (FromJSON k, FromJSON v) => FromJSON (Map_t k v)
instance (ToJSON k,   ToJSON v)   => ToJSON   (Map_t k v)

-- instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Map_t k v) where
--          parseJSON (Array arr) = do l <- M.forM (V.toList arr) parseJSON
--                                     return (M $ Map.fromList l)
--          parseJSON _           = mzero

-- instance (Ord k, ToJSON k, ToJSON v) => ToJSON (Map_t k v) where
--          toJSON (M m) = toJSON (Map.toList m)
