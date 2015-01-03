module Control.Monad.Trans.RWS.Strict.Wiring(
  transmuteRWST
) where

import Control.Monad.Wiring
import Control.Monad.Trans.RWS.Strict

transmuteRWST :: (Wirable w1 w2, Wirable r1 r2, Functor f) => RWST r2 w1 s f a -> RWST r1 w2 s f a
transmuteRWST = withRWST (\r -> \s -> (wire r, s)) . mapRWST (fmap (\(a, s, w) -> (a, s, wire w)))