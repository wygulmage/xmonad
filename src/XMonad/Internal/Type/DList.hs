{-# LANGUAGE
    TypeFamilies
  , ScopedTypeVariables
  #-}

module XMonad.Internal.Type.DList where

import qualified Data.Foldable as Monoid
import Data.Semigroup
import Data.Monoid
import Data.Coerce (coerce)
import qualified GHC.Exts as Exts

newtype DList a = DList{ runDList :: [a] -> [a] }

instance Exts.IsList (DList a) where
    type Item (DList a) = a
    toList = toList
    fromList = DList . (<>)

instance Monoid.Foldable DList where
    toList = toList
    foldr f z = foldr f z . toList

instance Traversable DList where
    traverse f = fmap fromList . traverse f . toList

instance Semigroup (DList a) where
    (<>) = coerce (flip (.) :: ([a] -> [a]) -> ([a] -> [a]) -> [a] -> [a])

instance Monoid (DList a) where
    mempty = DList id

instance Functor DList where
    fmap f = fromList . fmap f . toList

instance Applicative DList where
    pure = DList . (:)
    fs <*> xs = fromList (toList fs <*> toList xs)

instance Monad DList where
    xs >>= f = Monoid.fold (fmap f xs)

cons :: a -> DList a -> DList a
cons x = (pure x <>)

snoc :: a -> DList a -> DList a
snoc x xs = xs <> pure x

fromList :: [a] -> DList a
fromList = DList . (<>)
{-# INLINE [1] fromList #-}

toList :: DList a -> [a]
toList = (`id` []) . runDList
{-# INLINE [1] toList #-}


{-# RULES
  "fromList . toList = id "
  forall x. fromList (toList x) = x
  ;
  "toList . fromList = id"
  forall x. toList (fromList x) = x
  #-}
