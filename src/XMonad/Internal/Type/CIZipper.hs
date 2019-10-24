{-# LANGUAGE
    TypeFamilies
  #-}

module XMonad.Internal.Type.CIZipper where

import Data.Foldable
import GHC.Exts (IsList (Item, fromList, fromListN))
import qualified GHC.Exts as Exts
import XMonad.Internal.Class.Zipper

data CIZipper a = Unchecked
    {-# UNPACK #-} !Int -- length
    {-# UNPACK #-} !Int -- focus index
    [a] -- non-empty list

-- cizipper :: (Int -> Int -> a -> b -> [a] -> c) -> (a -> b -> b) -> b -> Int -> Int -> a -> [a] -> c
-- cizipper f g z0 n ix = loop z0 ix
--     where
--     loop z i x xs
--         | 0 >= i = f n ix x z xs
--         | x' : xs' <- xs = loop (g x z) (i + 1) x' xs'
--         | otherwise = error "index out of bounds"


instance Zipper (CIZipper a) where
    type Item (CIZipper a) = a
    _focus f = fromCIZipperWith (\ n i x xu xd -> Unchecked n i . xu . (: xd) <$> f x) consDL id
    _up f = fromCIZipperWith (\ n i x xu xd -> Unchecked n i . foldl' (flip (:)) (x : xd) <$> f xu) (:) []
    _dn f = fromCIZipperWith (\ n i x xu xd -> Unchecked n i . xu . (x :) <$> f xd) consDL id

instance Foldable CIZipper where
    toList (Unchecked _ _ xs) = xs
    foldr f z = foldr f z . toList
    length (Unchecked n _ _) = n


fromCIZipperWith ::
   (Int -> Int -> a -> b -> [a] -> c) -> -- Combine size, focus index, focus, accumulated up, and down.
   (a -> b -> b) -> -- Accumulate up.
   b -> -- empty up accumulator
   CIZipper a -> c
fromCIZipperWith f g z (Unchecked n i xs) =
   loop i z xs
   where
   loop 0 xu (x : xd) = f n i x xu xd -- i is the initial index of the focus.
   loop j xu (x : xd) = loop (j - 1) (g x xu) xd
   loop _ _ _ = error "out-of-bounds or empty"


consDL :: a -> ([a] -> [a]) -> [a] -> [a]
consDL x xs = xs . (x :)
