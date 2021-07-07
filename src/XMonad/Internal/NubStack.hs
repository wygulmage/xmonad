{-# LANGUAGE GeneralizedNewtypeDeriving
  #-}

module XMonad.Internal.NubStack where

import Data.Foldable
import qualified Data.Set as Set

import qualified XMonad.Internal.Stack as Stack


newtype NubStack a = NubStack (Stack.Stack a)
  deriving (Eq, Foldable)

insertUp :: (Eq a)=> a -> Maybe (NubStack a) -> NubStack a
insertUp x (Just (NubStack xs))
  | elem x xs = NubStack xs
  | otherwise = NubStack $ Stack.insertUp x (Just xs)
insertUp x Nothing = singleton x

singleton :: a -> NubStack a
singleton x = NubStack $ Stack.Stack x [] []

-- mapFromStack should, first, preserve the focused item, and, second, preserve the top item. Although in practice it should be an expensive no-op.
mapFromStack :: (Ord b)=> (a -> b) -> Stack.Stack a -> NubStack b
mapFromStack f (Stack.Stack x0 upx dnx) = NubStack $ Stack.Stack x0 upx' dnx'
  where
    y0 = f x0
    upx' = fromList (Set.singleton y0) upx
    dnx' = fromList (Set.fromList (y0 : upx')) dnx
    fromList seen (x' : xs)
      | elem y seen = fromList seen xs
      | otherwise = y : fromList (Set.insert y seen) xs
      where y = f x'
    fromList _ _ = []
