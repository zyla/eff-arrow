{-# LANGUAGE Arrows, GADTs, RankNTypes, TypeApplications, ScopedTypeVariables, DerivingStrategies, GeneralizedNewtypeDeriving, KindSignatures #-}

module EffM where

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

-- | Request to execute an operation.
-- Note that the set of possible operations is not reflected in the type.
data Request o where
  Request
    :: Typeable e -- ^ to identify the request type at runtime
    => e i o'     -- ^ The request. It is expected to be a GADT
                  -- with type parameters for both input and output.
    -> i          -- ^ The input
    -> (o' -> o)  -- ^ Arbitrary output transformation function, because
                  -- Free requires this to be a functor
    -> Request o

instance Functor Request where
  fmap f (Request request input g) = Request request input (fmap f g)

type EffM = Free Request

send :: forall e i o. Typeable e => e i o -> i -> EffM o
send request input = liftF (Request request input id)

-- * Running EffM

newtype Handler m e = Handler { runHandler :: forall i o. e i o -> i -> m o }

newtype HandlerMap (m :: * -> *) = HandlerMap { unHandlerMap :: Map TypeRep Dynamic }
  deriving newtype (Semigroup, Monoid)

-- | Construct a singleton HandlerMap
handler :: forall m e. (Typeable m, Typeable e) => (forall i o. e i o -> i -> m o) -> HandlerMap m
handler h = HandlerMap (Map.singleton (typeRep (Proxy @e)) (toDyn (Handler h)))

run :: forall m a. (Typeable m, Monad m) => HandlerMap m -> EffM a -> m a
run handlers = foldFree $ \(Request (request :: e i o) input f) ->
  case Map.lookup (typeRep (Proxy @e)) (unHandlerMap handlers) of
    Nothing -> error "handler not found"
    Just dyn ->
      let handler = fromMaybe (error "handler is of the wrong type") (fromDynamic dyn :: Maybe (Handler m e))
      in f <$> runHandler handler request input
