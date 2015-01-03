module Control.Monad.Trans.Writer.Lazy.Wiring(
  demoteWriter
) where

import Control.Monad.Wiring
import Control.Monad.Trans.Writer.Lazy

demoteWriter :: (Wirable w1 w2, Functor f) => WriterT w1 f a -> WriterT w2 f a
demoteWriter = mapWriterT (fmap (\(a, w) -> (a, wire w)))