{-# LANGUAGE
    NoImplicitPrelude
  #-}

module XMonad.Zipper
  -- For now just export what's needed to compile.
  ( Stack (..)
  , filter, integrate, integrate', differentiate, focusDown, focusUp, reverse, swapUp
  ) where

import Prelude
  ( Eq (..), Ord (..), Num (..), Show (..), Read (..)
  , Bool (..), Int
  , ($), flip, uncurry
  , (||), otherwise
  )
import Control.Applicative
import Control.Category
import Control.Monad
import Data.Functor
import Data.Semigroup
import Data.Monoid
import Data.Foldable
import Data.Traversable
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Lens.Micro (Lens')
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Internal as Lens

-- |
-- A stack is a cursor onto a window list.
-- The data structure tracks focus by construction, and
-- the master window is by convention the top-most item.
-- Focus operations will not reorder the list that results from
-- flattening the cursor. The structure can be envisaged as:
--
-- >    +-- master:  < '7' >
-- > up |            [ '2' ]
-- >    +---------   [ '3' ]
-- > focus:          < '4' >
-- > dn +----------- [ '8' ]
--
-- A 'Stack' can be viewed as a list with a hole punched in it to make
-- the focused position. Under the zipper\/calculus view of such
-- structures, it is the differentiation of a [a], and integrating it
-- back has a natural implementation used in 'index'.
--
data Stack a = Stack { focus  :: !a        -- focused thing in this set
                     , up     :: [a]       -- clowns to the left
                     , down   :: [a] }     -- jokers to the right
    deriving (Show, Read, Eq)

----- Optics and Accessors -----

_focus :: Lens' (Stack a) a
_focus f s = (\ x' -> s{ focus = x' }) <$> f (focus s)

_up, _dn :: Lens' (Stack a) [a]
_up f s = (\ x' -> s{ up = x' }) <$> f (up s)
_dn f s = (\ x' -> s{ down = x' }) <$> f (down s)

(!?) :: Alternative m => Stack a -> Int -> m a
-- Safe indexing. Focus is at 0.
Stack x xu xd !? i
  | i == 0 = pure x
  | i < 0 = xu @? abs i
  | otherwise = xd @? (i - 1)
  where
  [] @? _ = empty
  (x' : _) @? 0 = pure x'
  (_ : xs) @? i' = xs @? (i' - 1)

----- Constructors and Converters -----

singleton :: a -> Stack a
singleton x = Stack x [] []

fromNonEmpty :: NonEmpty a -> Stack a
-- This is the true form of 'integrate'.
fromNonEmpty (x :| xs) = Stack x [] xs

toNonEmpty :: Stack a -> NonEmpty a
-- This is the hypothetical 'differentiate''.
toNonEmpty (Stack x xu xd) =
  case List.reverse xu of
  (x' : xs) -> x' :| xs <> (x : xd)
  _ -> x :| xd

----- Instances -----

instance Foldable Stack where
  foldMap = foldMapDefault

instance Functor Stack where
  fmap = fmapDefault

instance Traversable Stack where
  -- Traverse a zipper from top to bottom.
  traverse f (Stack x xu xd) =
    flip Stack <$> backwards xu <*> f x <*> traverse f xd
    where
    backwards = foldr consM (pure [])
       where consM y mys = mys <**> ((:) <$> f y)

instance Semigroup (Stack a) where
  Stack x xu xd <> xs = Stack x xu (xd <> toList xs)

instance Applicative Stack where
  pure x = Stack x [] []
  Stack f fu fd <*> xs@(Stack x xu xd) =
    Stack (f x)
      (fmap f xu <> (fu <*> toList xs))
      (fmap f xd <> (fd <*> toList xs))

instance Monad Stack where
  Stack x xu xd >>= f =
    case f x of
    Stack x' xu' xd' ->
      Stack x'
        (xu' <> foldMap (toList . f) xu)
        (xd' <> foldMap (toList . f) xd)


----- Specialized Methods -----

zipWith :: (a -> b -> c) -> Stack a -> Stack b -> Stack c
-- Zip by matching foci.
zipWith f (Stack x xu xd) (Stack y yu yd) = Stack
  (f x y)
  (List.reverse (List.zipWith f (List.reverse xu) (List.reverse yu)))
  (List.zipWith f xd yd)

--- Comonad:
-- extract = focus

extend :: (Stack a -> b) -> Stack a -> Stack b
-- A stack of all the possible refocusings of s, with s focused.
extend f s = Stack (f s) (goUp s) (goDn s)
  where
  goUp (Stack x (x' : xu') xd) = let s' = Stack x' xu' (x : xd) in f s' : goUp s'
  goUp _ = []
  goDn (Stack x xu (x' : xd')) = let s' = Stack x' (x : xu) xd' in f s' : goDn s'
  goDn _ = []


duplicate :: Stack a -> Stack (Stack a)
-- A stack of all the possible refocusings of s, with s focused.
duplicate = extend id

----- Functions -----

--- Endomorphisms ---

--- Inserting :: a -> Stack a -> Stack a, or a -> Maybe (Stack a) -> Stack a
-- There are a lot of ways to insert an element into a stack.
-- * You can insert a new focus and move the old focus up. (O 1)
-- * You can insert a new focus and move the old focus down. (O 1)
-- * You can insert above the focus. (O 1)
-- * You can insert below the focus. (O 1)
-- * You can insert onto the top of the stack (cons). (O n)
-- * You can insert below the bottom of the stack (snoc). (O n)
insertUp, insertDn :: a -> Stack a -> Stack a
insertUp x = Lens.over _up (x :)
insertDn x = Lens.over _dn (x :)

insert :: a -> Stack a -> Stack a
-- To match behavior of StackSet insertUp, move the old focus down.
insert x' (Stack x xu xd) = Stack x' xu (x : xd)

cons :: a -> Stack a -> Stack a
cons x' (Stack x xu xd) = Stack x (List.reverse (x' : List.reverse xu)) xd

focusUp, focusDown :: Stack a -> Stack a
focusUp (Stack t (l:ls) rs) = Stack l ls (t:rs)
focusUp (Stack t _      rs) = Stack x xs [] where (x:xs) = List.reverse (t:rs)
focusDown                   = reverse . focusUp . reverse

swapUp :: Stack a -> Stack a
swapUp  (Stack t (l:ls) rs) = Stack t ls (l:rs)
swapUp  (Stack t []     rs) = Stack t (List.reverse rs) []

-- | reverse a stack: up becomes down and down becomes up.
reverse :: Stack a -> Stack a
reverse (Stack t ls rs) = Stack t rs ls

------- Maybe (Stack a) -> Stack a: adding items ------
mInsert :: a -> Maybe (Stack a) -> Stack a
mInsert x' = maybe (singleton x') (insert x')

mCons :: a -> Maybe (Stack a) -> Stack a
mCons x' = maybe (singleton x') (cons x')

------ Stack a -> Maybe (Stack a): deleting items ------

-- Deleting :: Stack a -> Maybe (Stack a), or Maybe (Stack a) -> Maybe (Stack a)
-- There a lot of ways to delete an element from the stack.
-- You can delete the focus and try to replace it from above, then below.
-- You can delete the focus and try to replace it from below, then above.
-- You can delete the top of the stack.
-- You can delete the bottom of the stack.
deleteFocus, deleteTop :: Alternative m => Stack a -> m (Stack a)
-- Delete the focus and try to replace it from below, then above. (per StackSet delete behavior)
deleteFocus (Stack _ xu (x' : xd')) = pure (Stack x' xu xd')
deleteFocus (Stack _ (x' : xu') _) = pure (Stack x' xu' [])
deleteFocus _ = empty

deleteTop (Stack x xu xd) =
  case List.reverse (List.drop 1 (List.reverse (x : xu))) of
  (x' : xu') -> pure (Stack x' xu' xd)
  _ ->
    case xd of
    (x' : xd') -> pure (Stack x' [] xd')
    _ -> empty

-- |
-- /O(n)/. 'filter p s' returns the elements of 's' such that 'p' evaluates to
-- 'True'.  Order is preserved, and focus moves as described for 'delete'.
--
filter :: Alternative m => (a -> Bool) -> Stack a -> m (Stack a)
-- filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
-- filter p (Stack f ls rs) = case List.filter p (f:rs) of
--     f':rs' -> Just (Stack f' (List.filter p ls) rs')   -- maybe move focus down
--     []     -> case List.filter p ls of                  -- filter back up
--                     f':ls' -> Just (Stack f' ls' []) -- else up
--                     []     -> Nothing
filter p (Stack x xu xd) =
  case xd' of
  x' : xd'' -> pure (Stack x' xu' xd'')
  _ ->
    case xu' of
    x' : xu'' -> pure (Stack x' xu''[])
    _ -> empty
  where
    xu' = List.filter p xu
    xd' = List.filter p (x : xd)

------- Possibly-empty Stacks ------

integrate' :: Foldable m => m (Stack a) -> [a]
integrate' = foldMap toList

-- differentiate :: Alternative m => [a] -> m (Stack a)
differentiate :: (Foldable m, Alternative n) => m a -> n (Stack a)
differentiate = dx . toList
  where
  dx :: Alternative n => [a] -> n (Stack a)
  dx (x : xs) = pure (Stack x [] xs)
  dx _ = empty


-- mDeleteFocus :: Maybe (Stack a) -> Maybe (Stack a)
mDeleteFocus :: (Monad m, Alternative m) => m (Stack a) -> m (Stack a)
mDeleteFocus = (=<<) deleteFocus

-- mDeleteTop :: Maybe (Stack a) -> Maybe (Stack a)
mDeleteTop :: (Monad m, Alternative m) => m (Stack a) -> m (Stack a)
mDeleteTop = (=<<) deleteTop

-- mFilter :: (a -> Bool) -> Maybe (Stack a) -> Maybe (Stack a)
mFilter :: (Monad m, Alternative m) => (a -> Bool) -> m (Stack a) -> m (Stack a)
mFilter p = (=<<) (filter p)


------- Cruft -------

integrate :: Stack a -> [a]
integrate = toList
