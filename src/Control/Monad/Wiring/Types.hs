{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances #-}

module Control.Monad.Wiring.Types(
    Wirable,
    wire
  ) where

class Wirable a b where
  -- | Unambiguous conversion from a to b.
  wire :: a -> b

-- | The "identity" Wirable instance.
instance Wirable a a where
  wire a = a

-- | Unit can be obtained from anything.
instance Wirable a () where
  wire _ = ()