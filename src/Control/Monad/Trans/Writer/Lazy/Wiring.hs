{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.Writer.Lazy.Wiring(
  Wirable(..)
) where

import Control.Monad.Wiring
import Control.Monad.Trans.Writer.Lazy

instance (Wirable w1 w2, Functor f) => Wirable (WriterT w1 f a) (WriterT w2 f a) where
  wire = mapWriterT (fmap (\(a, w) -> (a, wire w)))