{-# LANGUAGE
    NoImplicitPrelude
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
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
type Zipper = Stack -- 'Zipper' is misnamed 'Stack', but it's too late to change that. (Zippers are not stacks. '[a]' is a stack of 'a'; 'Zipper a' is a cursor into a stack of 'a'.)
data Stack a = Stack { focus  :: !a        -- focused thing in this set
                     , up     :: [a]       -- clowns to the left
                     , down   :: [a] }     -- jokers to the right
    deriving (Show, Read, Eq)

----- Optics and Accessors -----

class HasFocus ta a | ta -> a where
  _focus :: Lens' ta a

instance HasFocus (Zipper a) a where
  _focus f s = (\ x' -> s{ focus = x' }) <$> f (focus s)

class HasUp ta a | ta -> a where
  _up :: Lens' ta [a]

instance HasUp (Zipper a) a where
  _up f s = (\ x' -> s{ up = x' }) <$> f (up s)

class HasDn ta a | ta -> a where
  _dn :: Lens' ta [a]

instance HasDn (Zipper a) a where
  _dn f s = (\ x' -> s{ down = x' }) <$> f (down s)

(!?) :: Alternative m => Zipper a -> Int -> m a
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

singleton :: a -> Zipper a
singleton x = Stack x [] []

fromNonEmpty :: NonEmpty a -> Zipper a
-- This is the true form of 'integrate'.
fromNonEmpty (x :| xs) = Stack x [] xs

toNonEmpty :: Zipper a -> NonEmpty a
-- This is the hypothetical 'differentiate''.
toNonEmpty (Stack x xu xd) =
  case List.reverse xu of
  (x' : xs) -> x' :| xs <> (x : xd)
  _ -> x :| xd

----- Instances -----

instance Foldable Zipper where
  foldMap = foldMapDefault

instance Functor Zipper where
  fmap = fmapDefault

instance Traversable Zipper where
  -- Traverse a zipper from top to bottom.
  traverse f (Stack x xu xd) =
    flip Stack <$> backwards xu <*> f x <*> traverse f xd
    where
    backwards = foldr consM (pure [])
       where consM y mys = mys <**> ((:) <$> f y)

instance Semigroup (Zipper a) where
  Stack x xu xd <> xs = Stack x xu (xd <> toList xs)

instance Applicative Zipper where
  pure x = Stack x [] []
  Stack f fu fd <*> xs@(Stack x xu xd) =
    Stack (f x)
      (fmap f xu <> (fu <*> toList xs))
      (fmap f xd <> (fd <*> toList xs))

instance Monad Zipper where
  Stack x xu xd >>= f =
    case f x of
    Stack x' xu' xd' ->
      Stack x'
        (xu' <> foldMap (toList . f) xu)
        (xd' <> foldMap (toList . f) xd)


----- Specialized Methods -----

zipWith :: (a -> b -> c) -> Zipper a -> Zipper b -> Zipper c
-- Zip by matching foci.
zipWith f (Stack x xu xd) (Stack y yu yd) = Stack
  (f x y)
  (List.reverse (List.zipWith f (List.reverse xu) (List.reverse yu)))
  (List.zipWith f xd yd)

--- Comonad:
-- extract = focus

extend :: (Zipper a -> b) -> Zipper a -> Zipper b
-- A stack of all the possible refocusings of s, with s focused.
extend f s = Stack (f s) (goUp s) (goDn s)
  where
  goUp (Stack x (x' : xu') xd) = let s' = Stack x' xu' (x : xd) in f s' : goUp s'
  goUp _ = []
  goDn (Stack x xu (x' : xd')) = let s' = Stack x' (x : xu) xd' in f s' : goDn s'
  goDn _ = []


duplicate :: Zipper a -> Zipper (Zipper a)
-- A stack of all the possible refocusings of s, with s focused.
duplicate = extend id

----- Functions -----

--- Endomorphisms ---

--- Inserting :: a -> Zipper a -> Zipper a, or a -> Maybe (Zipper a) -> Zipper a
-- There are a lot of ways to insert an element into a stack.
-- * You can insert a new focus and move the old focus up. (O 1)
-- * You can insert a new focus and move the old focus down. (O 1)
-- * You can insert above the focus. (O 1)
-- * You can insert below the focus. (O 1)
-- * You can insert onto the top of the stack (cons). (O n)
-- * You can insert below the bottom of the stack (snoc). (O n)
insertUp, insertDn :: a -> Zipper a -> Zipper a
insertUp x = Lens.over _up (x :)
insertDn x = Lens.over _dn (x :)

insert :: a -> Zipper a -> Zipper a
-- To match behavior of StackSet insertUp, move the old focus down.
insert x' (Stack x xu xd) = Stack x' xu (x : xd)

cons :: a -> Zipper a -> Zipper a
cons x' (Stack x xu xd) = Stack x (List.reverse (x' : List.reverse xu)) xd

focusUp, focusDown :: Zipper a -> Zipper a
focusUp (Stack t (l:ls) rs) = Stack l ls (t:rs)
focusUp (Stack t _      rs) = Stack x xs [] where (x:xs) = List.reverse (t:rs)
focusDown                   = reverse . focusUp . reverse

swapUp :: Zipper a -> Zipper a
swapUp  (Stack t (l:ls) rs) = Stack t ls (l:rs)
swapUp  (Stack t []     rs) = Stack t (List.reverse rs) []

-- | reverse a stack: up becomes down and down becomes up.
reverse :: Zipper a -> Zipper a
reverse (Stack t ls rs) = Stack t rs ls

------- Maybe (Zipper a) -> Zipper a: adding items ------
mInsert :: a -> Maybe (Zipper a) -> Zipper a
mInsert x' = maybe (singleton x') (insert x')

mCons :: a -> Maybe (Zipper a) -> Zipper a
mCons x' = maybe (singleton x') (cons x')

------ Zipper a -> Maybe (Zipper a): deleting items ------

-- Deleting :: Zipper a -> Maybe (Zipper a), or Maybe (Zipper a) -> Maybe (Zipper a)
-- There a lot of ways to delete an element from the stack.
-- You can delete the focus and try to replace it from above, then below.
-- You can delete the focus and try to replace it from below, then above.
-- You can delete the top of the stack.
-- You can delete the bottom of the stack.
deleteFocus, deleteTop :: Alternative m => Zipper a -> m (Zipper a)
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
filter :: Alternative m => (a -> Bool) -> Zipper a -> m (Zipper a)
-- filter :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
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

------- Possibly-empty Zipper ------

integrate' :: Foldable m => m (Zipper a) -> [a]
integrate' = foldMap toList

-- differentiate :: Alternative m => [a] -> m (Zipper a)
differentiate :: (Foldable m, Alternative n) => m a -> n (Zipper a)
differentiate = dx . toList
  where
  dx :: Alternative n => [a] -> n (Zipper a)
  dx (x : xs) = pure (Stack x [] xs)
  dx _ = empty


-- mDeleteFocus :: Maybe (Zipper a) -> Maybe (Zipper a)
mDeleteFocus :: (Monad m, Alternative m) => m (Zipper a) -> m (Zipper a)
mDeleteFocus = (=<<) deleteFocus

-- mDeleteTop :: Maybe (Zipper a) -> Maybe (Zipper a)
mDeleteTop :: (Monad m, Alternative m) => m (Zipper a) -> m (Zipper a)
mDeleteTop = (=<<) deleteTop

-- mFilter :: (a -> Bool) -> Maybe (Zipper a) -> Maybe (Zipper a)
mFilter :: (Monad m, Alternative m) => (a -> Bool) -> m (Zipper a) -> m (Zipper a)
mFilter p = (=<<) (filter p)


------- Cruft -------

integrate :: Zipper a -> [a]
integrate = toList
