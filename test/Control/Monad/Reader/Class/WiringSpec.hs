{-# LANGUAGE OverloadedStrings, TemplateHaskell, ScopedTypeVariables #-}

module Control.Monad.Reader.Class.WiringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck()
import Data.Functor.Identity
import Data.Monoid
import Control.Monad.Wiring
import qualified Control.Monad.Reader as R
import Control.Monad.Reader.Class
import Control.Monad.Reader.Class.Wiring

spec :: Spec
spec = do
  describe "Control.Monad.Reader.Class.Wiring" $ do
    describe "wiredAsk" $ do
      prop "Compose wire and ask" $ do
        (\r -> 
          let readerT   = (do
                            (i :: Int) <- wiredAsk
                            return i) :: R.ReaderT (Double, Int) Identity Int
              result    = runIdentity $ R.runReaderT readerT $ r
          in  result `shouldBe` (snd r))