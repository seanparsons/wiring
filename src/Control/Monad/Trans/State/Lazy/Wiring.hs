{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.State.Lazy.Wiring(
  Wirable(..)
) where

import Data.Monoid
import Control.Monad.Wiring
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS

instance (Monoid w, Functor f) => Wirable (StateT s f a) (RWSL.RWST r w s f a) where
  wire ste = RWSL.RWST $ (\_ -> \s -> fmap (\(a, is) -> (a, is, mempty)) $ runStateT ste $ s)

instance (Monoid w, Functor f) => Wirable (StateT s f a) (RWSS.RWST r w s f a) where
  wire ste = RWSS.RWST $ (\_ -> \s -> fmap (\(a, is) -> (a, is, mempty)) $ runStateT ste $ s)