{-# LANGUAGE OverloadedStrings, TemplateHaskell, ScopedTypeVariables #-}

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

data Database1 = Database1 deriving Show
data Resource1 = Resource1 deriving Show
data Database2 = Database2 deriving Show
data User = User String deriving Show

describeOrders :: User -> [String] -> String
describeOrders user orders = "User " ++ (show user) ++ " ordered " ++ (show orders)

userLookup :: Int -> ReaderT (Resource1, Database1) Identity User
userLookup userId = ReaderT (\_ -> Identity $ User ("testuser" ++ show userId))

ordersLookup :: Int -> ReaderT (Database2, Resource1) Identity [String]
ordersLookup userId = ReaderT (\_ -> Identity $ ["Cake"])

composedLookup :: Int -> ReaderT (Resource1, Database1, Database2) Identity String
composedLookup userId = do
  user    <- wire $ userLookup userId
  orders  <- wire $ ordersLookup userId
  return $ describeOrders user orders

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
    prop "Example of basic reader wiring" $ do
      (\userId -> 
        let result = runIdentity $ runReaderT (composedLookup userId) (Resource1, Database1, Database2)
        in  result `shouldBe` (describeOrders (User ("testuser" ++ show userId)) ["Cake"]))