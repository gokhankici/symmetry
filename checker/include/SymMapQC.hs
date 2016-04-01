{-# LANGUAGE DeriveGeneric #-}
module SymMap where

import qualified Data.Set    as S
import qualified Data.List   as L
import           Data.Maybe
import           Data.Aeson
import           Data.Vector as V
import           GHC.Generics

data Map_t k v = M [(k,v)]
                 deriving (Show, Generic)

instance ( Eq k
         , Eq v
         , Ord k
         , Ord v )
         => Eq (Map_t k v) where
  (M l1) == (M l2) = S.fromList l1 == S.fromList l2

{-@ embed Map_t as Map_t @-}
{-@ measure Map_select :: Map_t k v -> k -> v @-}
{-@ measure Map_store :: Map_t k v -> k -> v -> Map_t k v @-}
 
{-@ get :: m:Map_t k v -> k:k -> {v:v | v = Map_select m k} @-}
get        :: (Ord k, DefaultMap v) =>
              Map_t k v -> k -> v
get (M l) k = let r = L.find ((k ==) . fst) l
              in maybe def snd r

class DefaultMap v where
  def :: v

{-@ put :: m:Map_t k v -> k:k -> v:v -> {vv:Map_t k v | vv = Map_store m k v } @-}
put          :: (Ord k) => Map_t k v -> k -> v -> Map_t k v
put (M l) k v = M ((k,v) : (L.filter ((k /=) . fst) l))

emptyMap      = M []
singleton k v = M [(k,v)]

-- export / import as list of (key,value)
instance (ToJSON k,   ToJSON v) => ToJSON (Map_t k v) where
    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance (FromJSON k, FromJSON v) => FromJSON (Map_t k v)
    -- No need to provide a parseJSON implementation.














-- instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Map_t k v) where
--          parseJSON (Array arr) = do l <- M.forM (V.toList arr) parseJSON
--                                     return (M $ Map.fromList l)
--          parseJSON _           = mzero

-- instance (Ord k, ToJSON k, ToJSON v) => ToJSON (Map_t k v) where
--          toJSON (M m) = toJSON (Map.toList m)
