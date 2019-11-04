{-# LANGUAGE NoImplicitPrelude #-}

module XMonad.Type.Stack where

import Prelude (Eq, Read, Show, Bool, Maybe (..), flip)
import Control.Applicative (Applicative ((<*>), pure), (<**>))
import Control.Category (Category (..))
import Data.Foldable (Foldable (toList, foldr, foldl', foldMap))
import Data.Functor
import Data.Functor.Reverse (Reverse (Reverse, getReverse))
import Data.Semigroup
import Data.Traversable (Traversable (traverse))
import qualified Data.List as List

data Stack a = Stack [a] !a [a]
    deriving (Show, Read, Eq)

focus :: Functor m => (a -> m a) -> Stack a -> m (Stack a)
focus f ~(Stack xu x xd) = (\ x' -> Stack xu x' xd) <$> f x

up, dn :: Functor m => ([a] -> m [a]) -> Stack a -> m (Stack a)
up f ~(Stack xu x xd) = (\ xu' -> Stack xu' x xd) <$> f xu
dn f ~(Stack xu x xd) = Stack xu x <$> f xd


instance Semigroup (Stack a) where
    ~(Stack xu x xd) <> xs = Stack xu x (xd <> toList xs)

instance Functor Stack where
    fmap f ~(Stack xu x xd) = Stack (fmap f xu) (f x) (fmap f xd)

instance Foldable Stack where
    foldr f z ~(Stack xu x xd) = foldr f (foldr f z (x : xd)) (Reverse xu)

instance Traversable Stack where
    traverse f ~(Stack xu x xd) =
        Stack
        <$> (getReverse <$> traverse f (Reverse xu))
        <*> f x
        <*> traverse f xd


-- |
-- /O(n)/. Turn a list into a possibly empty stack (i.e., a zipper):
-- the first element of the list is current, and the rest of the list
-- is down.
differentiate :: [a] -> Maybe (Stack a)
differentiate []     = Nothing
differentiate (x:xs) = Just (Stack [] x xs)

-- |
-- /O(n)/ Flatten a possibly empty stack into a list.
integrate' :: Maybe (Stack a) -> [a]
integrate' = foldMap toList


-- |
-- /O(n)/. 'filter p s' returns the elements of 's' such that 'p' evaluates to
-- 'True'.  Order is preserved, and focus moves as described for 'delete'.
--
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter p (Stack ls f rs) = case List.filter p (f:rs) of
    f':rs' -> Just (Stack (List.filter p ls) f' rs')    -- maybe move focus down
    []     -> case List.filter p ls of                  -- filter back up
                    f':ls' -> Just (Stack ls' f' []) -- else up
                    []     -> Nothing

focusUp, focusDown :: Stack a -> Stack a
focusUp (Stack (l:ls) t rs) = Stack ls l (t:rs)
focusUp (Stack [] t rs) = Stack xs x [] where (x:xs) = List.reverse (t:rs)
focusDown = reverse . focusUp . reverse

swapUp :: Stack a -> Stack a
swapUp  (Stack (l:ls) t rs) = Stack ls t (l:rs)
swapUp  (Stack [] x xd) = Stack (List.reverse xd) x []

-- | reverse a zipper: up becomes down and down becomes up.
reverse :: Stack a -> Stack a
reverse (Stack xu x xd) = Stack xd x xu
