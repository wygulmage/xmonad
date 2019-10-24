{-# LANGUAGE
    TypeFamilies
  #-}

module XMonad.Internal.Type.Zipper where

import Data.Foldable
import Data.Traversable
import XMonad.Internal.Class.Zipper

-- For legacy reasons, a Zipper is called a 'Stack'.
data Stack a = Stack
    { focus :: !a
    , up :: [a]
    , down :: [a]
    }

instance Zipper (Stack a) where
    type Item (Stack a) = a
    _focus f (Stack x xu xd) = (\ x' -> Stack x' xu xd) <$> f x
    _up f (Stack x xu xd) = (\ xu' -> Stack x xu' xd) <$> f xu
    _dn f (Stack x xu xd) = Stack x xu <$> f xd


instance Functor Stack where
    fmap f (Stack x xu xd) = Stack (f x) (fmap f xu) (fmap f xd)

instance Foldable Stack where
    foldr f z (Stack x xu xd) = foldl (flip f) (foldr f z (x : xd)) xu
    toList (Stack x xu xd) = foldl (flip (:)) (x : xd) xu
