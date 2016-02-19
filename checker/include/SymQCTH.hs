{-# LANGUAGE TemplateHaskell #-}

module SymQCTH where

import Language.Haskell.TH

testTH :: Name -> Q Exp
testTH n = do rfs <- fmap getRecordFields $ reify n
              litE . stringL $ show rfs

getRecordFields :: Info -> [(String, [(String, String)])]
getRecordFields (TyConI (DataD _ _ _ cons _)) = concatMap getRF' cons
getRecordFields _ = []

getRF' :: Con -> [(String, [(String, String)])]
getRF' (RecC name fields) = [(nameBase name, map getFieldInfo fields)]
getRF' _ = []

getFieldInfo :: (Name, Strict, Type) -> (String, String)
getFieldInfo (name, _, ty) = (nameBase name, show ty)
