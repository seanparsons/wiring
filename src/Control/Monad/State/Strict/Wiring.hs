{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.State.Strict.Wiring() where

import Data.Monoid
import Control.Monad.Wiring
import Control.Monad.State.Strict
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS

instance (Monoid w, Functor f) => Wirable (StateT s f a) (RWSL.RWST r w s f a) where
  wire ste = RWSL.RWST $ (\_ -> \s -> fmap (\(a, is) -> (a, is, mempty)) $ runStateT ste $ s)

instance (Monoid w, Functor f) => Wirable (StateT s f a) (RWSS.RWST r w s f a) where
  wire ste = RWSS.RWST $ (\_ -> \s -> fmap (\(a, is) -> (a, is, mempty)) $ runStateT ste $ s)