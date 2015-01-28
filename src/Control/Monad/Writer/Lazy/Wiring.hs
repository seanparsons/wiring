{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Writer.Lazy.Wiring(
  Wirable(..)
) where

import Control.Monad.Wiring
import Control.Monad.Writer.Lazy
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS

instance (Wirable w1 w2, Functor f) => Wirable (WriterT w1 f a) (WriterT w2 f a) where
  wire = mapWriterT (fmap (\(a, w) -> (a, wire w)))

instance (Functor f) => Wirable (WriterT w f a) (RWSL.RWST r w s f a) where
  wire writer = RWSL.RWST $ (\r -> \s -> fmap (\(a, w) -> (a, s, w)) $ runWriterT writer)

instance (Functor f) => Wirable (WriterT w f a) (RWSS.RWST r w s f a) where
  wire writer = RWSS.RWST $ (\r -> \s -> fmap (\(a, w) -> (a, s, w)) $ runWriterT writer)