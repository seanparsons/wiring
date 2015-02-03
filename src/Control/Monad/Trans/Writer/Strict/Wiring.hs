{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.Writer.Strict.Wiring(
  Wirable(..)
) where

import Control.Monad.Wiring
import Control.Monad.Trans.Writer.Strict
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS

instance (Wirable w1 w2, Functor f) => Wirable (WriterT w1 f a) (WriterT w2 f a) where
  wire = mapWriterT (fmap (\(a, w) -> (a, wire w)))

instance (Wirable w1 w2, Functor f) => Wirable (WriterT w1 f a) (RWSL.RWST r w2 s f a) where
  wire writer = RWSL.RWST $ (\r -> \s -> fmap (\(a, w) -> (a, s, wire w)) $ runWriterT writer)

instance (Wirable w1 w2, Functor f) => Wirable (WriterT w1 f a) (RWSS.RWST r w2 s f a) where
  wire writer = RWSS.RWST $ (\r -> \s -> fmap (\(a, w) -> (a, s, wire w)) $ runWriterT writer)