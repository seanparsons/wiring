{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Wiring.TH where

import Language.Haskell.TH
import Control.Monad

class Wirable a b where
  wire :: a -> b

maxTupleSize = 20

generateTupleElementWirables :: Q [Dec]
generateTupleElementWirables = fmap join $ sequence $ do
  tupleSize <- [1..maxTupleSize]
  let tupleElements = [1..tupleSize]
  tupleElement <- tupleElements
  let aName = mkName "a"
  let aPat = VarP aName
  let aExp = return $ VarE aName
  let tupleParams = return $ foldl (\working -> \x -> AppT working $ VarT $ mkName ("a" ++ show x)) (TupleT tupleSize) tupleElements
  let tupleElementType =  return $ VarT $ mkName ("a" ++ show tupleElement)
  let tupleLambdaParams = return $ TupP $ fmap (\x -> if x == tupleElement then aPat else WildP) tupleElements
  return [d| instance Wirable $tupleParams $tupleElementType where wire $tupleLambdaParams = $aExp |]

generateTupleWirables :: Q [Dec]
generateTupleWirables = sequence $ do
  tupleSize <- [2..maxTupleSize]
  let aName = mkName "a"
  let aPat = return $ VarP aName
  let wirableName = mkName "Wirable"
  let wireName = mkName "wire"
  let tupleElements = [1..tupleSize]
  let tupleShape = foldl (\working -> \x -> AppT working $ VarT $ mkName ("a" ++ show x)) (TupleT tupleSize) tupleElements
  let tupleInstances = fmap (\x -> ClassP wirableName [VarT aName, VarT $ mkName ("a" ++ show x)]) tupleElements
  let tupleConstruction = return $ TupE $ replicate tupleSize (AppE (VarE wireName) (VarE aName))
  return $ do 
    wireDec <- [d|wire $aPat = $tupleConstruction|]
    return $ InstanceD tupleInstances (AppT (AppT (VarT wirableName) (VarT aName)) tupleShape) wireDec
