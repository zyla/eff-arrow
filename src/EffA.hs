{-# LANGUAGE Arrows, GADTs, RankNTypes, TypeApplications, ScopedTypeVariables, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module EffA where

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

import qualified EffM
import EffM (EffM)

type Dependencies = RSet TypeRep

-- Expanded type: EffA i o ~ (Dependencies, i -> EffM o)
newtype EffA i o = EffA (StaticWriter Dependencies (Kleisli EffM) i o)
  deriving newtype (Category, Arrow, ArrowChoice)
  -- Note: no ArrowApply. Why?

send :: forall e i o. Typeable e => e i o -> EffA i o
send request = EffA $ StaticArrow $ LazyWriter
  (RSet.singleton (typeRep (Proxy @e)))
  (Kleisli (EffM.send request))

getCode :: EffA i o -> i -> EffM o
getCode (EffA (StaticArrow (LazyWriter _ (Kleisli code)))) = code

getDependencies' :: EffA i o -> Dependencies
getDependencies' (EffA (StaticArrow (LazyWriter deps _))) = deps

getDependencies :: EffA i o -> Set TypeRep
getDependencies = RSet.toSet . getDependencies'

run :: forall m i o. (Typeable m, Monad m) => EffM.HandlerMap m -> EffA i o -> i -> m o
run handlers eff input = EffM.run handlers (getCode eff input)








----------------

type StaticWriter w = StaticArrow (LazyWriter w)

data LazyWriter w a = LazyWriter { fst' :: w, snd' :: a }

instance Functor (LazyWriter w) where
  fmap f x = LazyWriter (fst' x) (f (snd' x))

instance Monoid w => Applicative (LazyWriter w) where
  pure = LazyWriter mempty
  f <*> x = LazyWriter (fst' f <> fst' x) (snd' f (snd' x))
