{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.Reader.Wiring(
  Wirable(..)
) where

import Control.Monad.Wiring
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS
import Data.Monoid

instance (Wirable r1 r2) => Wirable (ReaderT r2 f a) (ReaderT r1 f a) where
  wire = withReaderT wire

instance (Monoid w, Functor f) => Wirable (ReaderT r f a) (RWSL.RWST r w s f a) where
  wire reader = RWSL.RWST $ (\r -> \s -> fmap (\a -> (a, s, mempty)) $ runReaderT reader $ r)

instance (Monoid w, Functor f) => Wirable (ReaderT r f a) (RWSS.RWST r w s f a) where
  wire reader = RWSS.RWST $ (\r -> \s -> fmap (\a -> (a, s, mempty)) $ runReaderT reader $ r)