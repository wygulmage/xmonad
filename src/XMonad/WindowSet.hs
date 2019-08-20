{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeFamilies #-}

module XMonad.WindowSet where

import Control.Lens
import Control.DeepSeq (NFData)
import qualified Data.IntSet as IntSet
import Graphics.X11.Types (Window)
import Data.Coerce (coerce)
import Data.Typeable (Typeable)

newtype WindowSet = WindowSet IntSet.IntSet
    deriving (Eq, Ord, Show, Read, Typeable, NFData)

instance Wrapped WindowSet where
    type Unwrapped WindowSet = IntSet.IntSet
    -- _Wrapped' = iso (\(WindowSet ws) -> ws) WindowSet
    -- _Wrapped' = iso coerce WindowSet
    _Wrapped' = iso coerce coerce

empty :: WindowSet
empty = coerce IntSet.empty

singleton :: Window -> WindowSet
singleton = WindowSet . IntSet.singleton . fromIntegral

fromList :: [Window] -> WindowSet
fromList = WindowSet . IntSet.fromList . fmap fromIntegral

toList :: WindowSet -> [Window]
toList = fmap fromIntegral . IntSet.toList . coerce

insert :: Window -> WindowSet -> WindowSet
insert w = WindowSet . IntSet.insert (fromIntegral w) . coerce

delete :: Window -> WindowSet -> WindowSet
delete w = WindowSet . IntSet.delete (fromIntegral w) . coerce

member :: Window -> WindowSet -> Bool
member w = IntSet.member (fromIntegral w) . coerce

null :: WindowSet -> Bool
null = IntSet.null . coerce

size :: WindowSet -> Int
size = IntSet.size . coerce
