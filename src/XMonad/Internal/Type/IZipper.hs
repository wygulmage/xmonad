{-# LANGUAGE
    TypeFamilies
  #-}

module XMonad.Internal.Type.IZipper where

import Data.Foldable
import qualified XMonad.Internal.Class.Zipper as Class

data IZipper a = Unchecked
    {-# UNPACK #-} !Int -- The index of the 'top' of the zipper. Must be nonpositive, but that is not checked.
    [a] -- The items in the zipper. Must be non-empty, but that is not checked.

instance Class.Zipper (IZipper a) where
  type Item (IZipper a) = a
  _focus f = fromIZipper (\ i x xu xd -> Unchecked i . xu . (: xd) <$> f x) consDL id
  _up f = fromIZipper (\ i x xu xd -> Unchecked i . foldl' (flip (:)) (x : xd) <$> f xu) (:) []
  _dn f = fromIZipper (\ i x xu xd -> Unchecked i . xu . (x :) <$> f xd) consDL id

instance Foldable IZipper where
  foldr f z = foldr f z . toList
  toList (Unchecked _ xs) = xs

instance Functor IZipper where
    fmap f (Unchecked i xs) = Unchecked i (fmap f xs)

instance Traversable IZipper where
    traverse f (Unchecked i xs) = Unchecked i <$> traverse f xs


fromIZipper ::
   (Int -> a -> b -> [a] -> c) -> -- Combine focus, accumulated up, and down.
   (a -> b -> b) -> -- Accumulate up.
   b -> -- empty up accumulator
   IZipper a -> c
fromIZipper f g z (Unchecked i xs) =
   loop i z xs
   where
   loop 0 xu (x : xd) = f i x xu xd
   loop j xu (x : xd) = loop (j + 1) (g x xu) xd
   loop j _ _ = error ("fromIZipper: " <> show j <> " is out-of-bounds. ")



-- ifoldr1 :: (Int -> a -> b) -> (Int -> a -> b -> b) -> IZipper a -> b
-- ifoldr1 f g (Unchecked ix (x : xs)) = loop ix x xs
--     where
--     loop i y (y' : ys) = g i y (loop (i + 1) y' ys)
--     loop i y _ = f i y
-- ifoldr1 _ _ _ = error "empty"

------- Non-exported Utilities ------

consDL :: a -> ([a] -> [a]) -> [a] -> [a]
consDL x fxs = fxs . (x :)

-- snocDL :: a -> ([a] -> [a]) -> [a] -> [a]
-- snocDL x fxs = (x :) . fxs
