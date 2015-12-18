{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Reader.Class.Wiring(
  wiredAsk
) where

import Control.Monad.Wiring
import Control.Monad.Reader.Class

-- | Retrieves the monad environment like 'ask', but uses 'wire' to transform the environment to the required type.
wiredAsk :: (Functor m, MonadReader r1 m, Wirable r1 r2) => m r2
wiredAsk = fmap wire ask