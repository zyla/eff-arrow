{-# LANGUAGE GADTs #-}
module NoopLoop where

import EffA
import Control.Arrow

data NoOp i o where
  NoOp :: NoOp () ()

noopLoop :: EffA () ()
noopLoop = send NoOp >>> noopLoop
