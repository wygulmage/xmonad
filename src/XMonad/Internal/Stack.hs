{-# LANGUAGE DeriveFunctor #-}


module XMonad.Internal.Stack (
   Stack (..), _focus, _up, _down,
   filter, mapMaybe, mapAlt,
   reverse,
   tryUp, tryDown,
   goUp, goDown, swapUp,
   focusTop, shiftTop, swapTop,
   insertUp, insertUpMaybe,
   integrate, integrate', differentiate,
   ) where


import Prelude hiding (filter, reverse)

import Control.Applicative
import Data.Foldable
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Maybe as List (mapMaybe)

import XMonad.Internal.Backwards


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
    deriving (Show, Read, Eq, Functor)

instance Applicative Stack where
   pure x = Stack x [] []
   liftA2 f xs ys = xs >>= \ x -> fmap (f x) ys

instance Monad Stack where
   Stack x upx dnx >>= f = case f x of
       Stack x' upx' dnx' -> Stack
           x'
           ((upx >>= toList . f) <> upx')
           (dnx' <> (dnx >>= toList . f))

instance Foldable Stack where
    foldr f z xs = foldl (flip f) (focus xs `f` foldr f z (down xs)) (up xs)
    {-# INLINE foldr #-}
    foldMap f xs =
        foldr (\ x y -> y `mappend` f x) mempty (up xs)
        `mappend` f (focus xs)
        `mappend` foldMap f (down xs)
    {-# INLINABLE foldMap #-}
    -- foldMap' f xs =
    --     foldl' (\ y x -> f x `mappend` y) (foldl' (\ y x -> y `mappend` f x) (f (focus xs)) (down xs)) (up xs)
    elem x xs = x == focus xs || elem x (up xs) || elem x (down xs)

    -- Not used in stacks of Windows, but may be useful generally:
    minimum = fold1Commutative' min
    {-# INLINABLE minimum #-}
    maximum = fold1Commutative' max
    {-# INLINABLE maximum #-}

    toList xs = foldl' (flip (:)) (focus xs : down xs) (up xs)
    {-# INLINE toList #-}

fold1Commutative' :: (a -> a -> a) -> Stack a -> a
fold1Commutative' f xs =
    foldl' f (foldl' f (focus xs) (up xs)) (down xs)
{-# INLINE fold1Commutative' #-}

instance Traversable Stack where
    traverse f s =
        liftA3 (flip Stack)
            -- 'Backwards' applies the Applicative in reverse order.
            (forwards (traverse (Backwards . f) (up s)))
            (f (focus s))
            (traverse f (down s))
    {-# INLINE traverse #-}

-- Stack Optics
-- Stack Lenses
_focus :: (Functor m)=> (a -> m a) -> Stack a -> m (Stack a)
{- ^
@_focus@ is a @Lens@ from a 'Stack' to its 'focus'.
-}
_focus f xs = f (focus xs) <&> \ focus' -> xs{ focus = focus' }

_up :: (Functor m)=> ([a] -> m [a]) -> Stack a -> m (Stack a)
{- ^ @_up@ is a @Lens@ from a 'Stack' to the list of its 'up' elements in reverse order.

Use @(_up . traverse %~)@ to map a function over the 'up' elements of a 'Stack'.
-}
_up f sta = f (up sta) <&> \ up' -> sta{ up = up' }

_down :: (Functor m)=> ([a] -> m [a]) -> Stack a -> m (Stack a)
{- ^ @_down@ is a @Lens@ from a 'Stack' to the list of its 'down' elements.

Use @(_down . traverse %~)@ to map a function over the 'down' elements of a 'Stack'.
-}
_down f sta = f (down sta) <&> \ dn' -> sta{ down = dn' }


reverse :: Stack a -> Stack a
reverse (Stack x ups dns) = Stack x dns ups

tryUp :: Stack a -> Maybe (Stack a)
-- ^ Focus on the next higher item. If you're at the top already, return Nothing.
tryUp (Stack x (x' : ups) dns) = Just $ Stack x' ups (x : dns)
tryUp _                        = Nothing

tryDown :: Stack a -> Maybe (Stack a)
-- ^ Focus on the next lower item. If your're already at the bottom, return Nothing.
tryDown = fmap reverse . tryUp . reverse
-- tryDown (Stack x ups (x' : dns)) = Just $ Stack x' (x : ups) dns
-- tryDown _                        = Nothing

goUp :: Stack a -> Stack a
-- ^ Focus on the next element up. If you're at the top, treat the stack as a loop and focus on the bottom.
goUp (Stack x (x' : ups) dns) = Stack x' ups (x : dns)
goUp (Stack x [] dns) = case List.reverse $ x : dns of
   x' : ups' -> Stack x' ups' []
   _         -> errorWithoutStackTrace "goUp: impossible empty list"

goDown :: Stack a -> Stack a
-- ^ Focus on the next element down. If you're at the bottom, treat the stack as a loop and focus on the top.
goDown = reverse . goUp . reverse

swapUp :: Stack a -> Stack a
-- ^ Swap the focus with the item above it, keeping focus on the focus. If you're at the top, treat the stack as a loop.
swapUp (Stack x (x' : upx) dnx) = Stack x upx (x' : dnx)
swapUp (Stack x [] dnx) = Stack x (List.reverse dnx) []

swapTop :: Stack a -> Stack a
-- ^ Make the current focus the top by swapping it with the current top.
swapTop s@(Stack x ups dns) = case List.reverse ups of
   [] -> s
   x' : xs -> Stack x [] (xs <> (x' : dns))

shiftTop :: Stack a -> Stack a
-- ^ Place the focused element on the top of the stack.
shiftTop (Stack x ups dns) = Stack x [] (List.reverse ups <> dns)

focusTop :: Stack a -> Stack a
-- ^ Make the top the focus by shifting the whole stack down.
focusTop s@(Stack x ups dns) = case List.reverse ups of
    [] -> s
    x' : xs -> Stack x' [] (xs <> (x : dns))

maybeStack :: [a] -> [a] -> Maybe (Stack a)
-- ^ Construct a stack from a reversed list and a list.
maybeStack upx dnx = case dnx of
    x : dnx' -> Just $ Stack x upx dnx'
    []       -> case upx of
        x : upx' -> Just $ Stack x upx' []
        []       -> Nothing

-- |
-- /O(n)/. 'filter p s' returns the elements of 's' such that 'p' evaluates to
-- 'True'.  Order is preserved, and focus moves as described for 'delete'.
--
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter p xs =
   maybeStack (List.filter p (up xs)) (List.filter p (focus xs : down xs))

mapMaybe :: (a -> Maybe b) -> Stack a -> Maybe (Stack b)
-- ^ @'filter' p === mapMaybe (\ x -> if p x then Just x else Nothing)@
mapMaybe f xs =
    maybeStack (List.mapMaybe f (up xs)) (List.mapMaybe f (focus xs : down xs))

mapAlt ::
    (Alternative m)=> (a -> m b) -> Maybe (Stack a) -> m (Maybe (Stack b))
-- ^ @mapMaybe f xs === join . mapAlt f . Just@
mapAlt _ Nothing = pure Nothing
mapAlt f (Just xs) =
    liftA2 maybeStack
        (forwards $ mapAltList (Backwards . f) (up xs))
        (mapAltList f (focus xs : down xs))

-- wither ::
--     (Applicative m)=>
--     (a -> m (Maybe b)) -> Maybe (Stack a) -> m (Maybe (Stack b))
-- wither _ Nothing = pure Nothing
-- wither f (Just xs) =
--     liftA2 maybeStack
--         (forwards $ witherList (Backwards . f) (up xs))
--         (witherList f (focus xs : down xs))


mapAltList :: (Alternative m)=> (a -> m b) -> [a] -> m [b]
mapAltList f =
    witherList (optional . f)

witherList :: (Applicative m)=> (a -> m (Maybe b)) -> [a] -> m [b]
witherList f = foldr consM (pure [])
  where
    consM = liftA2 (maybe id (:)) . f

-- unionAlt :: (Alternative m)=> (a -> a -> a) -> m a -> m a -> m a
-- -- If the first action is empty, return the second; otherwise combine the contents of the first item and the second.
-- -- unionAlt f x empty === empty
-- -- unionAlt f empty x === x
-- unionAlt f = liftA2 (maybe id f) . optional
-- -- For example, unionAlt f :: Maybe a -> Maybe a -> Maybe a === maybe id (fmap . f)


insertUp :: a -> Stack a -> Stack a
insertUp x' ~(Stack x upx dnx) = Stack x' upx (x : dnx)

insertUpMaybe :: a -> Maybe (Stack a) -> Stack a
insertUpMaybe x' = maybe (pure x') (insertUp x')


-- |
-- /O(n)/. Flatten a 'Stack' into a list.
--
integrate :: Stack a -> [a]
integrate = toList

-- |
-- /O(n)/ Flatten a possibly empty stack into a list.
integrate' :: Maybe (Stack a) -> [a]
integrate' = foldMap integrate

-- |
-- /O(n)/. Turn a list into a possibly empty stack (i.e., a zipper):
-- the first element of the list is focus, and the rest of the list
-- is down.
differentiate :: [a] -> Maybe (Stack a)
differentiate = maybeStack []
