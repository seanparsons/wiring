{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Reader.Wiring(
  wiredAsk
) where

import Control.Monad.Wiring
import Control.Monad.Reader
import Control.Monad.Reader.Class()
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS
import Data.Monoid

instance (Wirable r1 r2) => Wirable (ReaderT r2 f a) (ReaderT r1 f a) where
  wire = withReaderT wire

instance (Monoid w, Functor f, Wirable r1 r2) => Wirable (ReaderT r2 f a) (RWSL.RWST r1 w s f a) where
  wire rdr = RWSL.RWST $ (\r -> \s -> fmap (\a -> (a, s, mempty)) $ runReaderT (withReaderT wire rdr) r)

instance (Monoid w, Functor f, Wirable r1 r2) => Wirable (ReaderT r2 f a) (RWSS.RWST r1 w s f a) where
  wire rdr = RWSS.RWST $ (\r -> \s -> fmap (\a -> (a, s, mempty)) $ runReaderT (withReaderT wire rdr) r)

-- | Retrieves the monad environment like 'ask', but uses 'wire' to transform the environment to the required type.
wiredAsk :: (Functor m, MonadReader r1 m, Wirable r1 r2) => m r2
wiredAsk = fmap wire ask