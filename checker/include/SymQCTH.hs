{-# LANGUAGE TemplateHaskell #-}

module SymQCTH where

import Language.Haskell.TH
import Data.Maybe
import SymVerify
import Data.Map.Strict as M hiding (map, filter)

testTH  :: Name -> Q Exp
testTH n = getRecordFields <$> reify n

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

allInts = concatMap (\(r,w,n) -> if isAbs n then [] else [r,w]) thisPtrs

mkStateAccessor :: Q [Dec]
mkStateAccessor =
  do ints <- funD n cs
     return [ints]
     where n          = mkName "s2int"
           cs         = map mkclause allInts
           mkclause f = clause [varP s, litP (StringL f)] (body f) []
           body f     = normalB $ appsE [varE (mkName f), varE s]
           s          = mkName "s"


temp = do d <- runQ mkStateAccessor
          putStrLn $ pprint d

-- ######################################################################
-- Helper functions
-- ######################################################################
find' k m = M.findWithDefault (error $ (show k) ++ " doesn't exist in " ++ (show m)) k m

isAbs n = snd $ find' n thisPids

getAbs n    = head $ filter ((== n) . snd) thisAbs
getClassN n = let (c,_,_) = fst (getAbs n) in c
getClassK n = let (_,k,_) = fst (getAbs n) in k

uncurry3 f (x,y,z) = f x y z
