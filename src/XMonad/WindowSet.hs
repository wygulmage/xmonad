{-# LANGUAGE TypeFamilies #-}

module XMonad.WindowSet where

import Control.Lens
import qualified Data.IntSet as IntSet
import Graphics.X11.Types (Window)

newtype WindowSet = WindowSet IntSet.IntSet

instance Wrapped WindowSet where
    type Unwrapped WindowSet = IntSet.IntSet
    _Wrapped' = iso (\(WindowSet ws) -> ws) WindowSet

empty :: WindowSet
empty = WindowSet IntSet.empty

singleton :: Window -> WindowSet
singleton = WindowSet . IntSet.singleton . fromIntegral

fromList :: [Window] -> WindowSet
fromList = WindowSet . IntSet.fromList . fmap fromIntegral

insert :: Window -> WindowSet -> WindowSet
insert w (WindowSet ws) = WindowSet (IntSet.insert (fromIntegral w) ws)

delete :: Window -> WindowSet -> WindowSet
delete w (WindowSet ws) = WindowSet (IntSet.delete (fromIntegral w) ws)

member :: Window -> WindowSet -> Bool
member w (WindowSet ws) = IntSet.member (fromIntegral w) ws
