{-# LANGUAGE Arrows, GADTs, RankNTypes, TypeApplications, ScopedTypeVariables, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Lib where

import Prelude hiding(id, print, (.))

import Data.Typeable
import Data.Dynamic
import Data.Set (Set)
import RSet (RSet)
import RSet as RSet
import Data.Dependent.Map (DMap)
import Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum)
import Control.Monad.Free
import Control.Arrow
import Control.Category
import Control.Arrow.Transformer.Static (StaticArrow(..))
import Data.Function (fix)

{-
newtype TypeRepOf a = TypeRepOf TypeRep

newtype Handler m e = Handler { runHandler :: forall i o. e i o -> i -> m o }

type HandlerMap m = DMap TypeRepOf (Handler m)

typeRepOf :: forall e. Typeable e => TypeRepOf e
typeRepOf = TypeRepOf (typeRep (Proxy @e))
-}

type Dependencies = RSet TypeRep

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

data LazyWriter w a = LazyWriter { fst' :: w, snd' :: a }

instance Functor (LazyWriter w) where
  fmap f x = LazyWriter (fst' x) (f (snd' x))

instance Monoid w => Applicative (LazyWriter w) where
  pure = LazyWriter mempty
  f <*> x = LazyWriter (fst' f <> fst' x) (snd' f (snd' x))

type StaticWriter w = StaticArrow (LazyWriter w)

---

newtype Eff i o = Eff (StaticWriter Dependencies (Kleisli (Free Request)) i o)
  deriving newtype (Category, Arrow, ArrowChoice)

send :: forall e i o. Typeable e => e i o -> Eff i o
send request = Eff $ StaticArrow $ LazyWriter
  (RSet.singleton (typeRep (Proxy @e)))
  (Kleisli (\i -> liftF (Request request i id)))

{-
recursive :: Eff i (Either i o) -> Eff i o
recursive (Eff (StaticArrow (LazyWriter deps (Kleisli fn))) = Eff (StaticArrow (deps, Kleisli $ fix $ \loop input -> do
  result <- fn input
  case result of
    Left input2 -> loop input2
    Right result -> pure result))
-}

getCode :: Eff i o -> i -> Free Request o
getCode (Eff (StaticArrow (LazyWriter _ (Kleisli code)))) = code

getDependencies' :: Eff i o -> Dependencies
getDependencies' (Eff (StaticArrow (LazyWriter deps _))) = deps


getDependencies :: Eff i o -> Set TypeRep
getDependencies = RSet.toSet . getDependencies'

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

data NoOp i o where
  NoOp :: NoOp () ()

repl :: Eff () ()
repl = proc () -> do
  line <- readLine -< ()
  if line /= "" then do
    response <- handleCommand -< line
    print -< response
  else returnA -< ()
  repl -< ()

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

looptest = RSet.singleton 1 <> looptest
