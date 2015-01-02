{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Wiring.TH where

import Control.Monad.Wiring.Types
import Language.Haskell.TH
import Control.Monad

maxTupleSize = 20
wirableName = mkName "Wirable"
wireName = mkName "wire"
aName = mkName "a"

aNameForIndex :: Int -> Name
aNameForIndex index = mkName ("a" ++ show index)

generateTupleElementWirables :: Q [Dec]
generateTupleElementWirables = return $ do
  tupleSize <- [1..maxTupleSize]
  let tupleElements = [1..tupleSize]
  tupleElement <- tupleElements
  let aPat = VarP aName
  let aExp = VarE aName
  let tupleParams = foldl (\working -> \x -> AppT working $ VarT $ aNameForIndex x) (TupleT tupleSize) tupleElements
  let wirableType = (AppT (AppT (VarT wirableName) tupleParams) (VarT $ aNameForIndex tupleElement))
  let tupleElementType = VarT $ mkName ("a" ++ show tupleElement)
  let tupleLambdaParams = TupP $ fmap (\x -> if x == tupleElement then aPat else WildP) tupleElements
  let decls = [FunD wireName [Clause [tupleLambdaParams] (NormalB aExp) []]]
  return $ InstanceD [] wirableType decls

generateTupleWirables :: Q [Dec]
generateTupleWirables = return $ do
  tupleSize <- [1..maxTupleSize]
  let aPat = VarP aName
  let tupleElements = [1..tupleSize]
  let tupleShape = foldl (\working -> \x -> AppT working $ VarT $ aNameForIndex x) (TupleT tupleSize) tupleElements
  let tupleInstances = fmap (\x -> ClassP wirableName [VarT aName, VarT $ aNameForIndex x]) tupleElements
  let tupleConstruction = TupE $ replicate tupleSize (AppE (VarE wireName) (VarE aName))
  let decls = [FunD wireName [Clause [aPat] (NormalB tupleConstruction) []]]
  return $ InstanceD tupleInstances (AppT (AppT (VarT wirableName) (VarT aName)) tupleShape) decls
