{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Control.Monad.WiringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck()
import Control.Monad.Wiring

spec :: Spec
spec = do
  describe "Wirable" $ do
    prop "(Double, Int, String) -> (String, Double)" $ do
      (\tuple -> 
        let (doubleElement, _, stringElement) = tuple
        in  wire tuple `shouldBe` (stringElement, doubleElement)) :: (Double, Int, String) -> Expectation
    prop "(Double, Int, String) -> (Double, Int, String)" $ do
      (\tuple -> 
        let (doubleElement, intElement, stringElement) = tuple
        in  wire tuple `shouldBe` (stringElement, intElement, doubleElement)) :: (Double, Int, String) -> Expectation
    prop "(Double, Int, String) -> Int" $ do
      (\tuple -> 
        let (_, intElement, _) = tuple
        in  wire tuple `shouldBe` intElement) :: (Double, Int, String) -> Expectation