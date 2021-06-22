{-# LANGUAGE DeriveFunctor #-}


module XMonad.Internal.Stack where


import Control.Applicative (Alternative, (<|>), liftA2, liftA3, optional)
import Control.Applicative.Backwards (Backwards (Backwards, forwards))
import Data.Foldable (find, foldl', foldr, foldr', toList)
import Data.Semigroup (Any (Any, getAny), Dual (Dual, getDual))
import Data.Functor.Compose (Compose (Compose, getCompose))
import qualified Data.List as List
import qualified Data.Maybe as List (mapMaybe)
import Data.Maybe (fromMaybe)


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

instance Foldable Stack where
    -- foldl f z ~(Stack x upx dnx) = foldr (flip f) z (reverse dnx <> (x : upx))
    -- foldl f z xs = foldr (flip f) (foldl f z (down xs) `f` focus xs) (up xs)
    -- foldl' f z ~(Stack x upx dnx) = foldl' f z (reverse upx <> (x : dnx))
    -- foldr f z ~(Stack x upx dnx) = foldr f z (reverse upx <> (x : dnx))
    foldr f z xs = foldl (flip f) (focus xs `f` foldr f z (down xs)) (up xs)
    -- foldr' f z ~(Stack x upx dnx) = foldl' (flip f) z (reverse dnx <> (x : upx))
    -- toList xs = reverse (up xs) <> (focus xs : down xs)
    foldMap f xs =
        getDual (foldMap (Dual . f) (up xs)) `mappend`
        f (focus xs) `mappend`
        foldMap f (down xs)
    elem x xs = x == focus xs || elem x (up xs) || elem x (down xs)
    length xs = 1 + length (up xs) + length (down xs)
    null _ = False

instance Traversable Stack where
    traverse f s =
        liftA3 (flip Stack)
            -- 'Backwards' applies the Applicative in reverse order.
            (forwards (traverse (Backwards . f) (up s)))
            (f (focus s))
            (traverse f (down s))

-- Stack Optics
-- Stack Lenses
_focus :: (Functor m)=> (a -> m a) -> Stack a -> m (Stack a)
{- ^
@_focus@ is a @Lens@ from a 'Stack' to its 'focus'.
-}
_focus f sta = fmap (\ foc' -> sta{ focus = foc' }) (f (focus sta))

_up :: (Functor m)=> ([a] -> m [a]) -> Stack a -> m (Stack a)
{- ^ @_up@ is a @Lens@ from a 'Stack' to the list of its 'up' elements in reverse order.

Use @(_up . traverse %~)@ to map a function over the 'up' elements of a 'Stack'.
-}
_up f sta = fmap (\ up' -> sta{ up = up' }) (f (up sta))

_down :: (Functor m)=> ([a] -> m [a]) -> Stack a -> m (Stack a)
{- ^ @_down@ is a @Lens@ from a 'Stack' to the list of its 'down' elements.

Use @(_down . traverse %~)@ to map a function over the 'down' elements of a 'Stack'.
-}
_down f sta = fmap (\ dn' -> sta{ down = dn' }) (f (down sta))


mapMaybe :: (a -> Maybe b) -> Stack a -> Maybe (Stack b)
mapMaybe f xs =
    maybeStack (List.mapMaybe f (up xs)) (List.mapMaybe f (focus xs : down xs))

maybeStack :: [a] -> [a] -> Maybe (Stack a)
maybeStack upx dnx = case dnx of
    x : dnx' -> Just $ Stack x upx dnx'
    []       -> case upx of
        x : upx' -> Just $ Stack x upx' []
        []       -> Nothing

mapAlt ::
    (Alternative m)=> (a -> m b) -> Maybe (Stack a) -> m (Maybe (Stack b))
mapAlt _ Nothing = pure Nothing
mapAlt f (Just xs) =
    liftA2 maybeStack
        (forwards $ mapAltList (Backwards . f) (up xs))
        (mapAltList f (focus xs : down xs))

wither ::
    (Applicative m)=>
    (a -> m (Maybe b)) -> Maybe (Stack a) -> m (Maybe (Stack b))
wither _ Nothing = pure Nothing
wither f (Just xs) =
    liftA2 maybeStack
        (forwards $ witherList (Backwards . f) (up xs))
        (witherList f (focus xs : down xs))


mapAltList :: (Alternative m)=> (a -> m b) -> [a] -> m [b]
mapAltList f =
    witherList (optional . f)

witherList :: (Applicative m)=> (a -> m (Maybe b)) -> [a] -> m [b]
witherList f = foldr consM (pure [])
  where
    consM = liftA2 (maybe id (:)) . f
