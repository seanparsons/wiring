{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Control.Monad.State.Lazy.WiringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck()
import Data.Functor.Identity
import Data.Monoid
import Control.Monad.Wiring
import Control.Monad.State.Lazy
import Control.Monad.State.Lazy.Wiring()
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS

spec :: Spec
spec = do
  describe "Control.Monad.State.Lazy.Wiring" $ do
    prop "Promote to Lazy RWST" $ do
      (\r -> \s -> \a ->
        let stateT    = StateT $ (\is -> Identity (a, is * 2)) :: StateT Double Identity String
            promoted  = wire $ stateT :: RWSL.RWST Int String Double Identity String
            result    = runIdentity $ RWSL.runRWST promoted r s
        in  result `shouldBe` (a, s * 2, mempty))
    prop "Promote to Strict RWST" $ do
      (\r -> \s -> \a ->
        let stateT    = StateT $ (\is -> Identity (a, is * 2)) :: StateT Double Identity String
            promoted  = wire $ stateT :: RWSS.RWST Int String Double Identity String
            result    = runIdentity $ RWSS.runRWST promoted r s
        in  result `shouldBe` (a, s * 2, mempty))