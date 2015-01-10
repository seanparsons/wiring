{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Control.Monad.WiringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad.Wiring

spec :: Spec
spec = do
  describe "Wirable" $
    prop "(Double, Int, String) -> (String, Double)" $ do
      (\tuple -> 
        let (doubleElement, _, stringElement) = tuple
        in  wire tuple `shouldBe` (stringElement, doubleElement)) :: (Double, Int, String) -> Expectation