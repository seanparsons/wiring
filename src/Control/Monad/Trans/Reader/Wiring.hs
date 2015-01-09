{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.Reader.Wiring(
  Wirable(..)
) where

import Control.Monad.Wiring
import Control.Monad.Trans.Reader

instance (Wirable r1 r2) => Wirable (ReaderT r2 m a) (ReaderT r1 m a) where
  wire = withReaderT wire
