{-# LANGUAGE
    NoImplicitPrelude
  #-}

module XMonad.Zipper where

import Prelude (Eq (..), Show (..), Read (..), Bool (..), ($), (||), flip, uncurry)
import Control.Applicative
import Control.Category
import Control.Monad ((=<<))
import Data.Functor
import Data.Semigroup
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

_focus :: Lens' (Stack a) a
_focus f s = (\ x' -> s{ focus = x' }) <$> f (focus s)

_up, _dn :: Lens' (Stack a) [a]
_up f s = (\ x' -> s{ up = x' }) <$> f (up s)
_dn f s = (\ x' -> s{ down = x' }) <$> f (down s)

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

-- Inserting :: a -> Stack a -> Stack a, or a -> Maybe (Stack a) -> Stack a
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
deleteFocus, deleteTop :: Stack a -> Maybe (Stack a)
-- Delete the focus and try to replace it from below, then above. (per StackSet delete behavior)
deleteFocus (Stack _ xu (x' : xd')) = Just (Stack x' xu xd')
deleteFocus (Stack _ (x' : xu') _) = Just (Stack x' xu' [])
deleteFocus _ = Nothing

deleteTop (Stack x xu xd) =
  case List.reverse (List.drop 1 (List.reverse (x : xu))) of
  (x' : xu') -> Just (Stack x' xu' xd)
  _ ->
    case xd of
    (x' : xd') -> Just (Stack x' [] xd')
    _ -> Nothing

-- |
-- /O(n)/. 'filter p s' returns the elements of 's' such that 'p' evaluates to
-- 'True'.  Order is preserved, and focus moves as described for 'delete'.
--
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
-- filter p (Stack f ls rs) = case List.filter p (f:rs) of
--     f':rs' -> Just (Stack f' (List.filter p ls) rs')   -- maybe move focus down
--     []     -> case List.filter p ls of                  -- filter back up
--                     f':ls' -> Just (Stack f' ls' []) -- else up
--                     []     -> Nothing
filter p (Stack x xu xd) =
  case xd' of
  x' : xd'' -> Just (Stack x' xu' xd'')
  _ ->
    case xu' of
    x' : xu'' -> Just (Stack x' xu''[])
    _ -> Nothing
  where
    xu' = List.filter p xu
    xd' = List.filter p (x : xd)

------- Possibly-empty Stacks ------

integrate' :: Maybe (Stack a) -> [a]
integrate' = maybe [] toList

differentiate :: [a] -> Maybe (Stack a)
differentiate (x : xs) = Just (Stack x [] xs)
differentiate _ = Nothing


mDeleteFocus :: Maybe (Stack a) -> Maybe (Stack a)
mDeleteFocus = (=<<) deleteFocus

mDeleteTop :: Maybe (Stack a) -> Maybe (Stack a)
mDeleteTop = (=<<) deleteTop

mFilter :: (a -> Bool) -> Maybe (Stack a) -> Maybe (Stack a)
mFilter p = (=<<) (filter p)


------- Cruft -------

integrate :: Stack a -> [a]
integrate = toList
