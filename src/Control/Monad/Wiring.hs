{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Wiring(
  Wirable(..),
  main
) where

import Control.Monad.Wiring.TH
import Control.Monad.Wiring.Types
import Control.Monad

$(generateTupleElementWirables)

$(generateTupleWirables)

test :: (Wirable (Int, String, Double) (Double, Int)) => (Int, String, Double) -> (Double, Int)
test = wire

main :: IO ()
main = print $ test (1, "Cake", 2.0)