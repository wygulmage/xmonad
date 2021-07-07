{-# LANGUAGE GeneralizedNewtypeDeriving
  #-}

module XMonad.Internal.NubListR (
  NubListR, singleton, cons, uncons, fromList, mapFromList, fmap, traverse
  ) where


import Prelude hiding (fmap, traverse)

import Data.Foldable
import qualified Data.Functor as Functor
import qualified Data.Traversable as Traversable
import qualified Data.Set as Set


newtype NubListR a = NubListR [a]
{- ^ A @NubList@ is an ordered collection without duplicate items. -}
  deriving (Eq, Foldable)


instance (Ord a)=> Semigroup (NubListR a) where
  NubListR xs1 <> NubListR xs2 = NubListR $ filter (`notElem` Set.fromList xs2) xs1 <> xs2
  {- ^ For duplicate items, the last is kept. -}

instance (Ord a)=> Monoid (NubListR a) where
  mempty = NubListR []

cons :: (Eq a)=> a -> NubListR a -> NubListR a
{- ^ For duplicate items, the original @NubListR@ is returned. -}
cons x (NubListR xs)
  | elem x xs = NubListR xs
  | otherwise = NubListR $ x : xs

uncons :: NubListR a -> Maybe (a, NubListR a)
uncons (NubListR (x : xs)) = Just (x, NubListR xs)
uncons _ = Nothing

singleton :: a -> NubListR a
singleton x = NubListR (x : [])

fromList :: (Ord a)=> [a] -> NubListR a
{- ^ For duplicate items, the last is kept. -}
-- fromList = foldMap (NubListR . (: []))
fromList = mapFromList id

mapFromList :: (Ord b)=> (a -> b) -> [a] -> NubListR b
{- ^ For duplicate items, the last is kept. -}
mapFromList f = NubListR . snd . foldr go (Set.empty, [])
  where
    go x z@ ~(unseen, ys) -- This is a bit crazy. It check the set of items you haven't folded over yet to decide whether to include an element. Need to check the stack behavior...
      | elem y unseen = z
      | otherwise   = (Set.insert y unseen, y : ys)
      where
        y = f x

fmap :: (Ord b)=> (a -> b) -> NubListR a -> NubListR b
fmap f = mapFromList f . toList

traverse :: (Applicative m, Ord b)=> (a -> m b) -> NubListR a -> m (NubListR b)
traverse f (NubListR xs) = Functor.fmap fromList (Traversable.traverse f xs)
