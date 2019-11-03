{-# LANGUAGE NoImplicitPrelude #-}

module XMonad.Zipper where

import Prelude (Bool, Maybe (..), flip)
import Control.Applicative (Applicative ((<*>), pure), (<**>))
import Control.Category (Category (..))
import Data.Foldable (Foldable (toList, foldr, foldl', foldMap))
import Data.Functor
import Data.Semigroup
import Data.Traversable (Traversable (traverse))
import qualified Data.List as List

data Zipper a = Zipper [a] !a [a]

focus :: Functor m => (a -> m a) -> Zipper a -> m (Zipper a)
focus f ~(Zipper xu x xd) = (\ x' -> Zipper xu x' xd) <$> f x

up, dn :: Functor m => ([a] -> m [a]) -> Zipper a -> m (Zipper a)
up f ~(Zipper xu x xd) = (\ xu' -> Zipper xu' x xd) <$> f xu
dn f ~(Zipper xu x xd) = Zipper xu x <$> f xd


instance Semigroup (Zipper a) where
    ~(Zipper xu x xd) <> xs = Zipper xu x (xd <> toList xs)

instance Functor Zipper where
    fmap f ~(Zipper xu x xd) = Zipper (fmap f xu) (f x) (fmap f xd)

instance Foldable Zipper where
    toList ~(Zipper xu x xd) = foldl' (flip (:)) (x : xd) xu
    foldr f z = foldr f z . toList

instance Traversable Zipper where
    traverse f ~(Zipper xu x xd) =
        Zipper
        <$> backwardsF xu
        <*> f x
        <*> traverse f xd
        where
          backwardsF (y : ys) = (:) <$> f y >*< backwardsF ys
          backwardsF _ = pure []
          (>*<) = flip (<**>)
          infixl 4 >*<


-- |
-- /O(n)/. Turn a list into a possibly empty stack (i.e., a zipper):
-- the first element of the list is current, and the rest of the list
-- is down.
differentiate :: [a] -> Maybe (Zipper a)
differentiate []     = Nothing
differentiate (x:xs) = Just (Zipper [] x xs)

-- |
-- /O(n)/ Flatten a possibly empty stack into a list.
integrate' :: Maybe (Zipper a) -> [a]
integrate' = foldMap toList


-- |
-- /O(n)/. 'filter p s' returns the elements of 's' such that 'p' evaluates to
-- 'True'.  Order is preserved, and focus moves as described for 'delete'.
--
filter :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
filter p (Zipper ls f rs) = case List.filter p (f:rs) of
    f':rs' -> Just (Zipper (List.filter p ls) f' rs')    -- maybe move focus down
    []     -> case List.filter p ls of                  -- filter back up
                    f':ls' -> Just (Zipper ls' f' []) -- else up
                    []     -> Nothing

focusUp, focusDown :: Zipper a -> Zipper a
focusUp (Zipper (l:ls) t rs) = Zipper ls l (t:rs)
focusUp (Zipper [] t rs) = Zipper xs x [] where (x:xs) = List.reverse (t:rs)
focusDown = reverse . focusUp . reverse

swapUp :: Zipper a -> Zipper a
swapUp  (Zipper (l:ls) t rs) = Zipper ls t (l:rs)
swapUp  (Zipper [] x xd) = Zipper (List.reverse xd) x []

-- | reverse a zipper: up becomes down and down becomes up.
reverse :: Zipper a -> Zipper a
reverse (Zipper xu x xd) = Zipper xd x xu
