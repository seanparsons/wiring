{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Wiring.TH where

import Control.Monad.Wiring.Types()
import Language.Haskell.TH
import Control.Monad()

maxTupleSize :: Int
maxTupleSize = 20

wirableName :: Name
wirableName = mkName "Wirable"

wireName :: Name
wireName = mkName "wire"

aName :: Name
aName = mkName "a"

aNameForIndex :: Int -> Name
aNameForIndex index = mkName ("a" ++ show index)

generateTupleElementWirables :: Q [Dec]
generateTupleElementWirables = return $ do
  tupleSize <- [2..maxTupleSize]
  let tupleElements = [1..tupleSize]
  tupleElement <- tupleElements
  let aPat = VarP aName
  let aExp = VarE aName
  let tupleParams = foldl (\working -> \x -> AppT working $ VarT $ aNameForIndex x) (TupleT tupleSize) tupleElements
  let wirableType = (AppT (AppT (ConT wirableName) tupleParams) (VarT $ aNameForIndex tupleElement))
  let tupleLambdaParams = TupP $ fmap (\x -> if x == tupleElement then aPat else WildP) tupleElements
  let decls = [FunD wireName [Clause [tupleLambdaParams] (NormalB aExp) []]]
  return $ InstanceD [] wirableType decls

generateTupleWirables :: Q [Dec]
generateTupleWirables = return $ do
  tupleSize <- [2..maxTupleSize]
  let aPat = VarP aName
  let tupleElements = [1..tupleSize]
  let tupleShape = foldl (\working -> \x -> AppT working $ VarT $ aNameForIndex x) (TupleT tupleSize) tupleElements
#if MIN_VERSION_base(4,9,0)
  let tupleInstances = fmap (\x -> foldl' AppT (ConT wirableName) [VarT aName, VarT $ aNameForIndex x]) tupleElements
#else
  let tupleInstances = fmap (\x -> ClassP wirableName [VarT aName, VarT $ aNameForIndex x]) tupleElements
#endif
  let tupleConstruction = TupE $ replicate tupleSize (AppE (VarE wireName) (VarE aName))
  let decls = [FunD wireName [Clause [aPat] (NormalB tupleConstruction) []]]
  return $ InstanceD tupleInstances (AppT (AppT (ConT wirableName) (VarT aName)) tupleShape) decls
