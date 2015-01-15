{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Control.Monad.Trans.State.Strict.WiringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Functor.Identity
import Data.Monoid
import Control.Monad.Wiring
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.State.Strict.Wiring
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS

spec :: Spec
spec = do
  describe "Control.Monad.Trans.State.Strict.Wiring" $ do
    prop "Promote to Lazy RWST" $ do
      (\r -> \s -> \a ->
        let stateT    = StateT $ (\s -> Identity (a, s * 2)) :: StateT Double Identity String
            promoted  = wire $ stateT :: RWSL.RWST Int String Double Identity String
            result    = runIdentity $ RWSL.runRWST promoted r s
        in  result `shouldBe` (a, s * 2, mempty))
    prop "Promote to Strict RWST" $ do
      (\r -> \s -> \a ->
        let stateT    = StateT $ (\s -> Identity (a, s * 2)) :: StateT Double Identity String
            promoted  = wire $ stateT :: RWSS.RWST Int String Double Identity String
            result    = runIdentity $ RWSS.runRWST promoted r s
        in  result `shouldBe` (a, s * 2, mempty))