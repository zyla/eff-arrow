{-#OPTIONS_GHC -ddump-ds #-}
module NoopLoop where

import Lib
import Control.Arrow

noopLoop :: Eff () ()
noopLoop = send NoOp >>> noopLoop
