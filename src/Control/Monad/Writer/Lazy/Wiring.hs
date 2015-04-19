{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Writer.Lazy.Wiring(
  wiredTell
) where

import Control.Monad.Wiring
import Control.Monad.Writer.Lazy
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS

instance (Wirable w1 w2, Functor f) => Wirable (WriterT w1 f a) (WriterT w2 f a) where
  wire = mapWriterT (fmap (\(a, w) -> (a, wire w)))

instance (Wirable w1 w2, Functor f) => Wirable (WriterT w1 f a) (RWSL.RWST r w2 s f a) where
  wire wrtr = RWSL.RWST $ (\_ -> \s -> fmap (\(a, w) -> (a, s, wire w)) $ runWriterT wrtr)

instance (Wirable w1 w2, Functor f) => Wirable (WriterT w1 f a) (RWSS.RWST r w2 s f a) where
  wire wrtr = RWSS.RWST $ (\_ -> \s -> fmap (\(a, w) -> (a, s, wire w)) $ runWriterT wrtr)

wiredTell :: (Monoid w2, Monad m, MonadWriter w2 m, Wirable w1 w2) => w1 -> m ()
wiredTell = tell . wire 