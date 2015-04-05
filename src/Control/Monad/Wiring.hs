{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Wiring(
  Wirable(..)
) where

import Control.Monad.Wiring.TH
import Control.Monad.Wiring.Types

$(generateTupleElementWirables)

$(generateTupleWirables)