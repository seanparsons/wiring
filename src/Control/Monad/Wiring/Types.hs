{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Wiring.Types(
    Wirable,
    wire
  ) where

class Wirable a b where
  wire :: a -> b