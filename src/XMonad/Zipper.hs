{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE PatternGuards          #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module XMonad.Zipper
  -- For now just export what's needed to compile.
    ( Stack(..)
    , filter
    , integrate
    , integrate'
    , differentiate
    , focusDown
    , focusUp
    , reverse
    , swapUp
    , _focus
    , swapTop
    , moveToTop
    , focusOnTop
    ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Foldable

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))

import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Traversable
import Lens.Micro (Lens')
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Internal as Lens
import Prelude
    ( Bool (..)
    , Eq (..)
    , Int
    , Num (..)
    , Ord (..)
    , Read (..)
    , Show (..)
    , error
    , flip
    , otherwise
    , uncurry
    , ($)
    , (||)
    )

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

data Stack a =
    Stack
        { focus :: !a -- focused thing in this set
        , up    :: [a] -- clowns to the left
        , down  :: [a] -- jokers to the right
        }
    deriving (Show, Read, Eq)

-- Alternate formulation without invariants 'baked in':
data IZipper (emptiability :: Emptiability) a =
    Unchecked !Int [a]
    deriving (Show, Read, Eq)

data Emptiability
    = Monoidal
    | NonEmpty

----- Optics and Accessors -----
class IsZipper ta a | ta -> a where
    _focus :: Lens' ta a
    _up :: Lens' ta [a]
    _dn :: Lens' ta [a]

instance IsZipper (Zipper a) a where
    _focus f s = (\x' -> s {focus = x'}) <$> f (focus s)
    _up f s = (\x' -> s {up = x'}) <$> f (up s)
    _dn f s = (\x' -> s {down = x'}) <$> f (down s)

instance IsZipper (IZipper 'NonEmpty a) a where
    _focus f =
        fromIZipperWith
            (\i x xu xd -> Unchecked i . xu . (: xd) <$> f x)
            consDL
            id
    _up f =
        fromIZipperWith
            (\i x xu xd ->
                 Unchecked i . List.foldl' (flip (:)) (x : xd) <$> f xu)
            (:)
            []
    _dn f =
        fromIZipperWith
            (\i x xu xd -> Unchecked i . xu . (x :) <$> f xd)
            consDL
            id

class Indexed m where
    (!?) :: Alternative n => m a -> Int -> n a

instance Indexed [] where
    (x:xs) !? i
        | 0 < i = xs !? (i - 1)
        | 0 == i = pure x
    _ !? _ = empty

instance Indexed Zipper where
    Stack x xu xd !? i
        | i == 0 = pure x
        | i < 0 = xu !? abs i
        | otherwise = xd !? (i - 1)

instance Indexed (IZipper any) where
    Unchecked o xs !? i = xs !? (o + i)

----- Constructors and Converters -----
singleton :: a -> Zipper a
singleton x = Stack x [] []

fromNonEmpty :: NonEmpty a -> Zipper a
-- This is the true form of 'integrate'.
fromNonEmpty (x :| xs) = Stack x [] xs

{-# INLINABLE [2] fromNonEmpty #-}
toNonEmpty :: Zipper a -> NonEmpty a
-- This is the hypothetical 'differentiate''.
toNonEmpty (Stack x xu xd) =
    case List.reverse xu of
        (x':xs) -> x' :| xs <> (x : xd)
        _       -> x :| xd

{-# INLINABLE [1] toNonEmpty #-}
----- Instances -----
instance Foldable Zipper where
    foldMap = foldMapDefault
    foldr f z = foldr f z . toList
    toList (Stack x xu xd) = foldl (flip (:)) (x : xd) xu

instance Functor Zipper where
    fmap f (Stack x xu xd) = Stack (f x) (fmap f xu) (fmap f xd)

-- Traverse a zipper from top to bottom.
instance Traversable Zipper where
    traverse f (Stack x xu xd) =
        flip Stack <$> backwards xu <*> f x <*> traverse f xd
      where
        backwards = foldr consM (pure [])
          where
            consM y mys = mys <**> ((:) <$> f y)

instance Semigroup (Zipper a) where
    Stack x xu xd <> xs = Stack x xu (xd <> toList xs)

instance Applicative Zipper where
    pure x = Stack x [] []
    Stack f fu fd <*> xs@(Stack x xu xd) =
        Stack
            (f x)
            (mapPrepend f xu (fu <*> toList (reverse xs)))
            (mapPrepend f xd (fd <*> toList xs))

instance Monad Zipper where
    Stack x xu xd >>= f
        | Stack x' xu' xd' <- f x = Stack
            x'
            (xu' <> foldMap (toList . f) xu)
            (xd' <> foldMap (toList . f) xd)

    -- Stack x xu xd >>= f =
    --     case f x of
    --         Stack x' xu' xd' ->
    --             Stack
    --                 x'
    --                 (xu' <> foldMap (toList . f) xu)
    --                 (xd' <> foldMap (toList . f) xd)

instance Foldable (IZipper any) where
    foldr f z = foldr f z . toList
    toList (Unchecked _ xs) = xs

instance Functor (IZipper any) where
    fmap f (Unchecked i xs) = Unchecked i (fmap f xs)

instance Traversable (IZipper any) where
    traverse f (Unchecked i xs) = Unchecked i <$> traverse f xs

instance Semigroup (IZipper any a) where
    (<>) (Unchecked i xs) = Unchecked i . (xs <>) . toList

instance Monoid (IZipper 'Monoidal a) where
    mempty = Unchecked 0 []

instance Applicative (IZipper 'NonEmpty) where
    pure = Unchecked 0 . pure
    fs <*> xs = zipperToIZipper (iZipperToZipper fs <*> iZipperToZipper xs)

-- Need to keep track of current and initial indices to return the index of the focused f applied to the focused f. (This will depend of the lengths of fs and xs, but we don't want to loop through the lists an extra time.)
-- Unchecked i fs <*> Unchecked j xs = loop i k fs xs
zipperToIZipper :: Zipper a -> IZipper any a
zipperToIZipper (Stack x xu xd) = loop 0 id xu
  where
    loop i a (y:ys) = loop (i + 1) (consDL y a) ys
    loop i a _      = Unchecked i (a (x : xd))

iZipperToZipper :: IZipper 'NonEmpty a -> Zipper a
iZipperToZipper = fromIZipperWith (pure Stack) (:) []

fromIZipperWith ::
       (Int -> a -> b -> [a] -> c) -- Combine focus, accumulated up, and down.
    -> (a -> b -> b) -- Accumulate up.
    -> b -- empty up accumulator
    -> IZipper 'NonEmpty a
    -> c
fromIZipperWith f g z0 (Unchecked i (x:xs)) = loop z0 i x xs
  where
    loop z j y (y':ys')
        | j > 0 = loop (g y z) (j - 1) y' ys'
    loop z j y ys = f j y z ys
fromIZipperWith _ _ _ _ = error "empty IZipper "

----- Specialized Methods -----
zipWith :: (a -> b -> c) -> Zipper a -> Zipper b -> Zipper c
-- Zip by matching foci.
zipWith f (Stack x xu xd) (Stack y yu yd) =
    Stack
        (f x y)
        (List.reverse (List.zipWith f (List.reverse xu) (List.reverse yu)))
        (List.zipWith f xd yd)

--- Comonad:
-- extract = Lens.view _focus
extend :: (Zipper a -> b) -> Zipper a -> Zipper b
-- A stack of all the possible refocusings of s, with s focused.
extend f s = Stack (f s) (goUp s) (goDn s)
  where
    goUp (Stack x (x':xu') xd) =
        let s' = Stack x' xu' (x : xd)
         in f s' : goUp s'
    goUp _ = []
    goDn (Stack x xu (x':xd')) =
        let s' = Stack x' (x : xu) xd'
         in f s' : goDn s'
    goDn _ = []

duplicate :: Zipper a -> Zipper (Zipper a)
-- A stack of all the possible refocusings of s, with s focused.
duplicate = extend id

--- Nonempty Foldable
foldr1By :: (a -> b) -> (a -> b -> b) -> Stack a -> b
foldr1By f g (Stack x xu xd) = foldl (flip g) (foldrList x xd) xu
  where
    foldrList y (y':ys) = g y (foldrList y' ys)
    foldrList y _       = f y

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
focusUp (Stack t (l:ls) rs) = Stack l ls (t : rs)
focusUp (Stack t _ rs) = Stack x xs []
  where
    (x:xs) = List.reverse (t : rs)

focusDown = reverse . focusUp . reverse

swapUp :: Zipper a -> Zipper a
swapUp (Stack t (l:ls) rs) = Stack t ls (l : rs)
swapUp (Stack t _ rs)      = Stack t (List.reverse rs) []

swapTop :: Zipper a -> Zipper a
-- Swap the top and the focus, keeping focus on the focus.
swapTop s@(Stack _ [] _) = s
swapTop (Stack x xu xd) = Stack x [] (xs <> (t : xd))
  where
    (t:xs) = List.reverse xu

moveToTop :: Zipper a -> Zipper a
moveToTop (Stack x xu xd) = Stack x [] (List.reverse xu <> xd)

focusOnTop :: Zipper a -> Zipper a
focusOnTop = fromNonEmpty . toNonEmpty

-- | reverse a stack: up becomes down and down becomes up.
reverse :: Zipper a -> Zipper a
reverse (Stack t ls rs) = Stack t rs ls

------- Maybe (Zipper a) -> Zipper a: adding items ------
mInsertBy :: Applicative m => (b -> a -> m b) -> b -> Maybe a -> m b
mInsertBy f x = maybe (pure x) (f x)

mInsert :: a -> Maybe (Zipper a) -> Zipper a
mInsert = mInsertBy insert

mCons :: a -> Maybe (Zipper a) -> Zipper a
mCons = mInsertBy cons

------ Zipper a -> Maybe (Zipper a): deleting items ------
-- Deleting :: Zipper a -> Maybe (Zipper a), or Maybe (Zipper a) -> Maybe (Zipper a)
-- There a lot of ways to delete an element from the stack.
-- You can delete the focus and try to replace it from above, then below.
-- You can delete the focus and try to replace it from below, then above.
-- You can delete the top of the stack.
-- You can delete the bottom of the stack.
deleteFocus, deleteTop :: Alternative m => Zipper a -> m (Zipper a)
-- Delete the focus and try to replace it from below, then above. (per StackSet delete behavior)
deleteFocus (Stack _ xu (x':xd')) = pure (Stack x' xu xd')
deleteFocus (Stack _ (x':xu') _)  = pure (Stack x' xu' [])
deleteFocus _                     = empty

deleteTop (Stack x xu xd)
    | x' : xu' <- (List.reverse . List.drop 1 . List.reverse) (x : xu)
        = pure (Stack x' xu' xd)
    | x' : xd' <- xd
        = pure (Stack x' [] xd')
    | otherwise
        = empty
-- deleteTop (Stack x xu xd) =
--     case List.reverse (List.drop 1 (List.reverse (x : xu))) of
--         (x':xu') -> pure (Stack x' xu' xd)
--         _ ->
--             case xd of
--                 (x':xd') -> pure (Stack x' [] xd')
--                 _        -> empty

-- |
-- /O(n)/. 'filter p s' returns the elements of 's' such that 'p' evaluates to
-- 'True'.  Order is preserved, and focus moves as described for 'delete'.
--
filter :: Alternative m => (a -> Bool) -> Zipper a -> m (Zipper a)
filter p (Stack x xu xd)
    | x' : xd'' <- xd' = pure (Stack x' xu' xd'')
    | x' : xu'' <- xu' = pure (Stack x' xu'' [])
    | otherwise        = empty
  where
    xu' = List.filter p xu
    xd' = List.filter p (x : xd)

------- Possibly-empty Zipper ------
integrate' :: Foldable m => m (Zipper a) -> [a]
integrate' = foldMap toList

differentiate :: (Foldable m, Alternative n) => m a -> n (Zipper a)
differentiate = dx . toList
  where
    dx :: Alternative n => [a] -> n (Zipper a)
    dx (x:xs) = pure (Stack x [] xs)
    dx _      = empty

------- Non-exported Utilities ------
consDL :: a -> ([a] -> [a]) -> [a] -> [a]
consDL x fxs = fxs . (x :)

-- snocDL :: a -> ([a] -> [a]) -> [a] -> [a]
-- snocDL x fxs = (x :) . fxs

mapPrepend :: (a -> b) -> [a] -> [b] -> [b]
mapPrepend g = flip (foldr ((:) . g))

------- Laws -------
{-# RULES
"toNonEmpty . fromNonEmpty = id" forall x .
                                 toNonEmpty (fromNonEmpty x) = x
 #-}

------- Cruft -------
integrate :: Zipper a -> [a]
integrate = toList

{-# DEPRECATED
integrate "Use 'Data.Foldable.toList'."
 #-}
