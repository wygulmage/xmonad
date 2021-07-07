{-# LANGUAGE GeneralizedNewtypeDeriving
  #-}

module XMonad.Internal.NubList (
  NubList, fromList, mapFromList, fmap, traverse, singleton, cons, cons', uncons,
  ) where


import Prelude hiding (fmap, traverse)

import Data.Foldable
import qualified Data.Functor as Functor
import qualified Data.Traversable as Traversable
import qualified Data.Set as Set


newtype NubList a = NubList [a]
{- ^ A @NubList@ is an ordered collection without duplicate items. -}
  deriving (Eq, Foldable)


instance (Ord a)=> Semigroup (NubList a) where
  NubList xs1 <> NubList xs2 =
      NubList $ xs1 <> filter (`notElem` Set.fromList xs1) xs2
  {- ^ For duplicate items, the first is kept. -}

instance (Ord a)=> Monoid (NubList a) where
  mempty = NubList []


cons :: (Eq a)=> a -> NubList a -> NubList a
cons x (NubList xs) = NubList $ x : filter (x /=) xs

uncons :: NubList a -> Maybe (a, NubList a)
uncons (NubList (x : xs)) = Just (x, NubList xs)
uncons _ = Nothing

singleton :: a -> NubList a
singleton x = NubList (x : [])

fromList :: (Ord a)=> [a] -> NubList a
fromList = mapFromList id

mapFromList :: (Ord b)=> (a -> b) -> [a] -> NubList b
-- mapFromList f = foldMap (NubList . (: []) . f)
mapFromList f = NubList . go Set.empty
  where
    go _    []       = []
    go seen (x : xs) = let y = f x in
      if elem y seen then go seen xs else y : go (Set.insert y seen) xs

fmap :: (Ord b)=> (a -> b) -> NubList a -> NubList b
fmap f = mapFromList f . toList

traverse :: (Applicative m, Ord b)=> (a -> m b) -> NubList a -> m (NubList b)
traverse f (NubList xs) = Functor.fmap fromList (Traversable.traverse f xs)
