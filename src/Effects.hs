{-# LANGUAGE Arrows, GADTs, RankNTypes, TypeApplications, ScopedTypeVariables, DerivingStrategies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase #-}

module Effects where

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
import System.IO (getLine)
import Data.IORef
import System.IO.Unsafe

import EffM

data Teletype i o where
  Print :: Teletype String ()
  ReadLine :: Teletype () String

data DB i o where
  Get :: DB String (Maybe String)
  Set :: DB (String, String) ()

db :: IORef (Map String String)
db = unsafePerformIO $ newIORef mempty
{-# NOINLINE db #-}

handlers :: HandlerMap IO
handlers = mconcat
  [ handler $ \request input ->
      case request of
        Print -> putStrLn input
        ReadLine -> getLine
  , handler $ \request ->
      case request of
        Get -> \key -> Map.lookup key <$> readIORef db
        Set -> \(key, value) -> do
          atomicModifyIORef db (\m -> (Map.insert key value m, ()))
          pure ()
  ]
