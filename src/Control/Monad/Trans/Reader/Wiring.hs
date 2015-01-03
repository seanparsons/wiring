module Control.Monad.Trans.Reader.Wiring(
  promoteReader
) where

import Control.Monad.Wiring
import Control.Monad.Trans.Reader

promoteReader :: (Wirable r1 r2) => ReaderT r2 m a -> ReaderT r1 m a
promoteReader = withReaderT wire