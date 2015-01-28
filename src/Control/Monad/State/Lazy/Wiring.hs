{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.State.Lazy.Wiring(
  Wirable(..)
) where

import Data.Monoid
import Control.Monad.Wiring
import Control.Monad.State.Lazy
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS

instance (Monoid w, Functor f) => Wirable (StateT s f a) (RWSL.RWST r w s f a) where
  wire state = RWSL.RWST $ (\_ -> \s -> fmap (\(a, s) -> (a, s, mempty)) $ runStateT state $ s)

instance (Monoid w, Functor f) => Wirable (StateT s f a) (RWSS.RWST r w s f a) where
  wire state = RWSS.RWST $ (\_ -> \s -> fmap (\(a, s) -> (a, s, mempty)) $ runStateT state $ s)