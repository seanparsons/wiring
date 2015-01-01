module Control.Monad.Trans.Reader.Wiring(
  readerWire
) where

import Control.Monad.Wiring
import Control.Monad.Trans.Reader

readerWire :: (Wirable r1 r2) => ReaderT r2 m a -> ReaderT r1 m a
readerWire = withReaderT wire