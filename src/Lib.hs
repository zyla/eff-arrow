{-# LANGUAGE Arrows, GADTs, RankNTypes, TypeApplications, ScopedTypeVariables, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Lib where

import Prelude hiding(id, print)

import Data.Typeable
import Data.Dynamic
import Data.Set (Set)
import Data.Set as Set
import Data.Dependent.Map (DMap)
import Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum)
import Control.Monad.Free
import Control.Arrow
import Control.Category
import Control.Arrow.Transformer.Static (StaticArrow(..))

{-
newtype TypeRepOf a = TypeRepOf TypeRep

newtype Handler m e = Handler { runHandler :: forall i o. e i o -> i -> m o }

type HandlerMap m = DMap TypeRepOf (Handler m)

typeRepOf :: forall e. Typeable e => TypeRepOf e
typeRepOf = TypeRepOf (typeRep (Proxy @e))
-}

type Dependencies = Set TypeRep

data Request o where
  Request :: Typeable e => e i o' -> i -> (o' -> o) -> Request o

instance Functor Request where
  fmap f (Request request input g) = Request request input (fmap f g)

--- Arrow with monoidal static part

{-
data StaticWriter w arr i o = StaticWriter w (arr i o)

instance (Monoid w, Category arr) => Category (StaticWriter w arr) where
  id = StaticWriter mempty id

instance (Monoid w, Arrow arr) => Arrow (StaticWriter w arr) where
  arr f = StaticWriter mempty (arr f)
  first (StaticWriter w a) = StaticWriter w (first a)
-}

type StaticWriter w = StaticArrow ((,) w)

---

newtype Eff i o = Eff (StaticWriter Dependencies (Kleisli (Free Request)) i o)
  deriving newtype (Category, Arrow, ArrowChoice)

send :: forall e i o. Typeable e => e i o -> Eff i o
send request = Eff $ StaticArrow (Set.singleton (typeRep (Proxy @e)), Kleisli (\i -> liftF (Request request i id)))

getDependencies :: Eff i o -> Dependencies
getDependencies (Eff (StaticArrow (deps, _))) = deps

data Teletype i o where
  Print :: Teletype String ()
  ReadLine :: Teletype () String

print = send Print
readLine = send ReadLine
get = send Get
set = send Set

data DB i o where
  Get :: DB String (Maybe String)
  Set :: DB (String, String) ()

repl :: Eff () ()
repl = proc () -> do
  line <- readLine -< ()
  if line /= "" then do
    response <- handleCommand -< line
    print -< response
  else returnA -< ()

handleCommand :: Eff String String
handleCommand = proc line ->
  case words line of
    ["get", key] -> do
      m_value <- get -< key
      returnA -< case m_value of
        Nothing    -> "Not found"
        Just value -> value
    ["set", key, value] -> do
      set -< (key, value)
      returnA -< ""
    _ -> do
      returnA -< "Invalid command"
