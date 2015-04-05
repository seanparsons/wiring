{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Writer.Lazy.WiringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Functor.Identity
import Data.Monoid
import Control.Monad.Wiring
import Control.Monad.Writer.Lazy
import Control.Monad.Writer.Lazy.Wiring
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS

spec :: Spec
spec = do
  describe "Control.Monad.Writer.Lazy" $ do
    describe "wiredTell" $ do
      prop "Compose wire and tell" $ do
        (\(pair :: (String, Double)) -> 
          let writerT   = wiredTell pair :: WriterT String Identity ()
              result    = runIdentity $ runWriterT writerT
          in  result `shouldBe` ((), fst pair))
    describe "Wirable" $ do
      prop "wire" $ do
        (\pair -> 
          let writerT   = WriterT $ Identity pair :: WriterT (String, Int) Identity Double
              demoted   = wire $ writerT :: WriterT String Identity Double
              result    = runIdentity $ runWriterT demoted
          in  result `shouldBe` (fst pair, fst $ snd pair))
      prop "Promote to Lazy RWST" $ do
        (\pair -> \r -> \s ->
          let writerT   = WriterT $ Identity pair :: WriterT (String, Int) Identity Double
              promoted  = wire $ writerT :: RWSL.RWST Double String String Identity Double
              result    = runIdentity $ RWSL.runRWST promoted r s
          in  result `shouldBe` (fst pair, s, fst $ snd pair))
      prop "Promote to Strict RWST" $ do
        (\pair -> \r -> \s ->
          let writerT   = WriterT $ Identity pair :: WriterT (String, Int) Identity Double
              promoted  = wire $ writerT :: RWSS.RWST Double String String Identity Double
              result    = runIdentity $ RWSS.runRWST promoted r s
          in  result `shouldBe` (fst pair, s, fst $ snd pair))