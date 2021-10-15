{-# LANGUAGE Arrows, GADTs, RankNTypes, TypeApplications, ScopedTypeVariables, DerivingStrategies, GeneralizedNewtypeDeriving, KindSignatures #-}

module ExamplesEffM where

import Prelude hiding(id, print, (.))

import Data.Typeable
import Data.Dynamic
import Data.Set (Set)
import RSet (RSet)
import RSet as RSet
import Data.Dependent.Map (DMap)
import Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Free
import Control.Arrow
import Control.Category
import Control.Arrow.Transformer.Static (StaticArrow(..))
import Data.Function (fix)
import Control.Monad (when)
import Data.Maybe
import Data.GADT.Compare

import EffM
import Effects

print = send Print
readLine = send ReadLine

get = send Get
set = send Set

repl :: EffM ()
repl = do
  line <- readLine ()
  when (line /= "") $ do
    response <- handleCommand line
    print response
  repl

handleCommand :: String -> EffM String
handleCommand line =
  case words line of
    ["get", key] -> do
      m_value <- get key
      pure $ case m_value of
        Nothing    -> "Not found"
        Just value -> value
    ["set", key, value] -> do
      set (key, value)
      pure ""
    _ -> do
      pure "Invalid command"
