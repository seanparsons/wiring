{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Wiring.TH where

import Language.Haskell.TH
import Control.Monad

class Wirable a b where
  wire :: a -> b

generateTupleElementWirables :: Q [Dec]
generateTupleElementWirables = fmap join $ sequence $ do
  tupleSize <- [1..20]
  let tupleElements = [1..tupleSize]
  tupleElement <- tupleElements
  let tupleParams = return $ foldl (\working -> \x -> AppT working $ VarT $ mkName ("a" ++ show x)) (TupleT tupleSize) tupleElements
  let tupleElementType =  return $ VarT $ mkName ("a" ++ show tupleElement)
  let aPat = VarP $ mkName "a"
  let aExp = return $ VarE $ mkName "a"
  let tupleLambdaParams = return $ TupP $ fmap (\x -> if x == tupleElement then aPat else WildP) tupleElements
  return $ [d| instance Wirable $tupleParams $tupleElementType where wire $tupleLambdaParams = $aExp |]
