{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Wiring.Types(
    Wirable,
    wire
  ) where

class Wirable a b where
  wire :: a -> b

instance Wirable a a where
  wire a = a

instance Wirable a () where
  wire _ = ()