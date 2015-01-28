{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.RWS.Strict.Wiring(
  Wirable(..)
) where

import Control.Monad.Wiring
import Control.Monad.Trans.RWS.Strict

instance (Wirable w1 w2, Wirable r1 r2, Functor f) => Wirable (RWST r2 w1 s f a) (RWST r1 w2 s f a) where
  wire = withRWST (\r -> \s -> (wire r, s)) . mapRWST (fmap (\(a, s, w) -> (a, s, wire w)))

