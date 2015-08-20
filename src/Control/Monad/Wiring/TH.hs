{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Wiring.TH where

import Control.Monad.Wiring.Types()
import Language.Haskell.TH
import Control.Monad()
import Data.List(foldl')
import Data.Traversable(traverse)

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

tupleSizes :: [Int]
tupleSizes = [2..maxTupleSize]

fName :: Name
fName = mkName "f"

bName :: Name
bName = mkName "b"

fPat :: PatQ
fPat = varP fName

fExp :: ExpQ
fExp = varE fName

arrowFromTo :: TypeQ -> TypeQ -> TypeQ
arrowFromTo from to = appT (appT arrowT from) to

arrowAllTheTypes :: [TypeQ] -> TypeQ -> TypeQ
arrowAllTheTypes (x : xs) lastType  = arrowFromTo x $ arrowAllTheTypes xs lastType
arrowAllTheTypes [] lastType        = lastType

functionOfNType :: Int -> TypeQ
functionOfNType n = arrowAllTheTypes (fmap (\x -> varT $ aNameForIndex x) [1..n]) (varT bName)

tupledFunctionType :: Int -> TypeQ
tupledFunctionType n = arrowFromTo (foldl' (\w -> \x -> appT w $ varT $ aNameForIndex x) (tupleT n) [1..n]) (varT bName)


generateTupleElementWirables :: Q [Dec]
generateTupleElementWirables = return $ do
  tupleSize <- tupleSizes
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
  tupleSize <- tupleSizes
  let aPat = VarP aName
  let tupleElements = [1..tupleSize]
  let tupleShape = foldl (\working -> \x -> AppT working $ VarT $ aNameForIndex x) (TupleT tupleSize) tupleElements
#if MIN_VERSION_template_haskell(2,10,0)
  let tupleInstances = fmap (\x -> foldl AppT (ConT wirableName) [VarT aName, VarT $ aNameForIndex x]) tupleElements
#else
  let tupleInstances = fmap (\x -> ClassP wirableName [VarT aName, VarT $ aNameForIndex x]) tupleElements
#endif
  let tupleConstruction = TupE $ replicate tupleSize (AppE (VarE wireName) (VarE aName))
  let decls = [FunD wireName [Clause [aPat] (NormalB tupleConstruction) []]]
  return $ InstanceD tupleInstances (AppT (AppT (ConT wirableName) (VarT aName)) tupleShape) decls

generateFunctionTuplingWirables :: Q [Dec]
generateFunctionTuplingWirables = do
  let wirableType n = appT (appT (conT wirableName) (functionOfNType n)) (tupledFunctionType n)
  let applyF n = lam1E (tupP $ fmap (\x -> varP $ aNameForIndex x) [1..n]) (foldl' (\w -> \x -> appE w (varE $ aNameForIndex x)) fExp [1..n])
  let wirableDecls n = [funD wireName [clause [fPat] (normalB $ applyF n) []]]
  traverse (\n -> instanceD (return []) (wirableType n) (wirableDecls n)) tupleSizes

generateFunctionUntuplingWirables :: Q [Dec]
generateFunctionUntuplingWirables = do
  let wirableType n = appT (appT (conT wirableName) (tupledFunctionType n)) (functionOfNType n)
  let applyF n = lamE (fmap (\x -> varP $ aNameForIndex x) [1..n]) (appE fExp (tupE $ fmap (\x -> varE $ aNameForIndex x) [1..n]))
  let wirableDecls n = [funD wireName [clause [fPat] (normalB $ applyF n) []]]
  traverse (\n -> instanceD (return []) (wirableType n) (wirableDecls n)) tupleSizes
