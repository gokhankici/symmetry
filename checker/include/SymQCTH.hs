{-# LANGUAGE TemplateHaskell #-}

module SymQCTH where

import Language.Haskell.TH
import Data.Maybe

testTH  :: Name -> Q Exp
testTH n = fmap getRecordFields $ reify n

getRecordFields :: Info -> Exp -- [(String, [(String, String)])]
getRecordFields (TyConI (DataD _ _ _ cons _)) = ListE (concatMap getRF' cons)
getRecordFields _                             = ListE []

getRF' :: Con -> [Exp] -- [(String, [(String, String)])]
getRF' (RecC name fields) = [TupE [ LitE . StringL $ nameBase name
                                  , ListE (map getFieldInfo fields)]]
getRF' _                  = []

getFieldInfo :: (Name, Strict, Type) -> Exp -- (String, String)
getFieldInfo (name, _, ty) = TupE [ LitE . StringL $ nameBase name
                                  , LitE . StringL $ show ty ]
