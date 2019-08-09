{-# LANGUAGE
    NoImplicitPrelude
  , TypeOperators
  #-}

module XMonad.Focused where

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
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe (..))


-- newtype Focused a = Focused ([a], a, [a])
newtype Focused a = Focused ([a], NonEmpty a)

focus :: Lens' (Focused a) a
focus f (Focused (up, x :| dn)) = (\ x' -> mkFocused up x' dn) <$> f x

mkFocused :: [a] -> a -> [a] -> Focused a
mkFocused ups x dns = Focused (ups, x :| dns)


----- Instances -----

instance Functor Focused where
    -- fmap f (Focused xup x xdn) = Focused (fmap f xup) (f x) (fmap f xdn)
    fmap = fmapDefault

instance Foldable Focused where
    foldMap = foldMapDefault

instance Traversable Focused where
    traverse f (Focused (up, dn)) =
        ((.).(.)) Focused (,) <$> backwards traverse f up <*> traverse f dn

instance Foldable1 Focused where
    foldMap1 = foldMap1Default
    toNonEmpty xs = xs'
        where (Focused (_, xs')) = focusToTop xs


instance Traversable1 Focused where
    traverse1 f (Focused (up : ups, dn)) = ((.).(.)) Focused (,) <$> fmap toList (traverse1 f (up :| ups)) <.> traverse1 f dn
    traverse1 f (Focused (_, dn)) = fromNonEmpty <$> traverse1 f dn

instance Semigroup (Focused a) where
    Focused (up, dn) <> xs = Focused (up, dn <> toNonEmpty xs)

----- Functions -----

fromNonEmpty :: NonEmpty a -> Focused a
-- ^ Focus on the head of a NonEmpty list (O(1)).
fromNonEmpty = Focused . (,) []

rotateUp :: Focused a -> Focused a
-- ^ Move the focus up, treating the whole Focused structure as a loop. O(n) worst case, O(1) average case.
rotateUp (Focused (x' : ups, x :| dn)) = Focused (ups, x' :| x : dn)
rotateUp (Focused (_, dn)) =
    case NonEmpty.reverse dn of
    x :| up -> Focused (up, x :| [])


focusToTop :: Focused a -> Focused a
-- ^ Shift the focus all the way up (O(n)).
focusToTop (Focused (x' : up, x :| dn)) = focusToTop (Focused (up, x' :| x : dn))
focusToTop xs = xs

reverse :: Focused a -> Focused a
reverse (Focused (up, x :| dn)) = Focused (dn, x :| up)


single :: a -> Focused a
-- ^ Focus on a single item (O(1)).
single = fromNonEmpty . pure

insert :: a -> Focused a -> Focused a
-- ^ Insert a new focus above the old (O(1)).
insert x' (Focused (up, x :| dn)) = Focused (up, x' :| x : dn)

delete :: Focused a -> Maybe (Focused a)
-- ^ Replace the focus with the item below it, if possible, or the item above it, or nothing (O(1)).
delete (Focused (up, _ :| x : dn)) = Just (Focused (up, x :| dn))
delete (Focused (x : up, _)) = Just (Focused (up, pure x))
delete _ = Nothing
