-- | A Set which can recursively depend on itself, provided the value is shared.
module RSet (RSet, fromSet, singleton, union, toSet) where

import Prelude hiding(id)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Unique
import System.IO.Unsafe

data RSet a
  = Leaf (Set a)
  | Union ID (RSet a) (RSet a)
  deriving (Show)

newtype ID = ID Unique deriving (Eq, Ord)

instance Show ID where show (ID u) = "<id: " <> show (hashUnique u) <> ">"

instance Semigroup (RSet a) where
  (<>) = union

instance Monoid (RSet a) where
  mempty = fromSet Set.empty

fromSet :: Set a -> RSet a
fromSet = Leaf

singleton :: a -> RSet a
singleton = fromSet . Set.singleton

union :: RSet a -> RSet a -> RSet a
union x y = Union (ID (unsafePerformIO newUnique)) x y
{-# NOINLINE union #-}

toSet :: Ord a => RSet a -> Set a
toSet = go Set.empty
  where
    go _ (Leaf x) = x
    go visited (Union id x y)
      | id `Set.member` visited = Set.empty
      | otherwise =
        let visited' = Set.insert id visited
        in go visited' x `Set.union` go visited' y
