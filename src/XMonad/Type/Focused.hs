{-# LANGUAGE
    NoImplicitPrelude
  , TypeOperators
  #-}

module XMonad.Type.Focused
    ( Focused, mkFocused, single
    , focus
    , fromNonEmpty
    , rotateUp, rotated, focusToTop
    , insert, delete
    ) where

import Control.Applicative (Applicative ((<*>), pure))
import Control.Category (Category ((.)))
import Control.Lens hiding ((<.>))
import Data.Functor (Functor (fmap), (<$>))
import Data.Functor.Apply ((<.>))
import Data.Foldable (Foldable (foldMap, toList))
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe (..))


-- newtype Focused a = Focused ([a], NonEmpty a)
newtype Focused a = Focused ([a], a, [a])

focus :: Lens' (Focused a) a
focus f (Focused (up, x, dn)) = (\ x' -> mkFocused up x' dn) <$> f x

mkFocused :: [a] -> a -> [a] -> Focused a
mkFocused up x dn = Focused (up, x, dn)


----- Instances -----

instance Functor Focused where
    fmap = fmapDefault

instance Foldable Focused where
    foldMap = foldMapDefault

instance Traversable Focused where
    traverse f (Focused (up, x, dn)) =
        mkFocused <$> backwards traverse f up <*> f x <*> traverse f dn

instance Foldable1 Focused where
    foldMap1 = foldMap1Default
    toNonEmpty xs = x :| xs'
        where (Focused (_, x, xs')) = focusToTop xs

instance Traversable1 Focused where
    traverse1 f (Focused (up : ups, x, dn)) = (\ up' (x' :| dn') -> mkFocused up' x' dn') <$> fmap toList (traverse1 f (up :| ups)) <.> traverse1 f (x :| dn)
    traverse1 f (Focused (_, x, dn)) = fromNonEmpty <$> traverse1 f (x :| dn)

instance Semigroup (Focused a) where
    Focused (up, x, dn) <> xs = mkFocused up x (dn <> toList xs)

instance Reversing (Focused a) where
    reversing (Focused (up, x, dn)) = mkFocused dn x up


----- Functions -----

fromNonEmpty :: NonEmpty a -> Focused a
-- ^ Focus on the head of a NonEmpty list (O(1)).
fromNonEmpty (x :| dn) = mkFocused [] x dn

rotateUp :: Focused a -> Focused a
-- ^ Move the focus up, treating the whole Focused structure as a loop. O(n) worst case, O(1) average case.
rotateUp (Focused (x' : ups, x, dn)) = mkFocused ups x' (x : dn)
rotateUp (Focused (_, x, dn)) =
    case NonEmpty.reverse (x :| dn) of
    x' :| up -> mkFocused up x' []

rotated :: Iso' (Focused a) (Focused a)
rotated = iso rotateUp (under reversed rotateUp)

focusToTop :: Focused a -> Focused a
-- ^ Shift the focus all the way up (O(n)).
focusToTop (Focused (x' : up, x, dn)) = focusToTop (mkFocused up x' (x : dn))
focusToTop xs = xs

single :: a -> Focused a
-- ^ Focus on a single item (O(1)).
single = fromNonEmpty . pure

insert :: a -> Focused a -> Focused a
-- ^ Insert a new focus above the old (O(1)).
insert x' (Focused (up, x, dn)) = mkFocused up x' (x : dn)

delete :: Focused a -> Maybe (Focused a)
-- ^ Replace the focus with the item below it, if possible, or the item above it, or nothing (O(1)).
delete (Focused (up, _, x : dn)) = Just (mkFocused up x dn)
delete (Focused (x : up, _, _)) = Just (mkFocused up x [])
delete _ = Nothing
