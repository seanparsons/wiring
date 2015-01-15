{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Control.Monad.Trans.Reader.WiringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Functor.Identity
import Data.Monoid
import Control.Monad.Wiring
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Reader.Wiring
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS

spec :: Spec
spec = do
  describe "Control.Monad.Trans.Reader.Wiring" $ do
    prop "Wirable" $ do
      (\r -> 
        let readerT   = ReaderT (\r -> Identity $ show r) :: ReaderT Double Identity String
            promoted  = wire $ readerT :: ReaderT (Double, Char) Identity String
            result    = runIdentity $ runReaderT promoted $ r
        in  result `shouldBe` (show $ fst r))
    prop "Promote to Lazy RWST" $ do
      (\r -> \s ->
        let readerT   = ReaderT (\r -> Identity $ show r) :: ReaderT Double Identity String
            promoted  = wire $ readerT :: RWSL.RWST Double String Double Identity String
            result    = runIdentity $ RWSL.runRWST promoted r s
        in  result `shouldBe` (show r, s, mempty))
    prop "Promote to Strict RWST" $ do
      (\r -> \s ->
        let readerT   = ReaderT (\r -> Identity $ show r) :: ReaderT Double Identity String
            promoted  = wire $ readerT :: RWSS.RWST Double String Double Identity String
            result    = runIdentity $ RWSS.runRWST promoted r s
        in  result `shouldBe` (show r, s, mempty))