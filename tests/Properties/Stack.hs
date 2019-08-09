{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Stack where

import Test.QuickCheck
import Instances

import XMonad.StackSet hiding (filter)
import qualified XMonad.StackSet as S (filter)

import Data.Maybe

import Control.Lens ((^.), (^..))
import qualified Control.Lens as Lens


-- The list returned by index should be the same length as the actual
-- windows kept in the zipper
prop_index_length :: T -> Bool
prop_index_length x =
    -- case stack . workspace . current $ x of
    case x^._currentStack of
    Nothing -> null (index x)
    Just it -> length (index x) == length it


-- For all windows in the stackSet, findTag should identify the
-- correct workspace
prop_findIndex :: T -> Bool
prop_findIndex x =
    and [ tag w == fromJust (findTag i x)
        | w <- x^.._workspaces
        -- , t <- maybeToList (stack w)
        , t <- toList (w^._stack)
        , i <- toList t
        ]

prop_allWindowsMember (NonEmptyWindowsStackSet x) = do
      -- Reimplementation of arbitraryWindow, but to make sure that
      -- implementation doesn't change in the future, and stop using allWindows,
      -- which is a key component in this test (together with member).
    let ws = allWindows x
    -- We know that there is at least 1 window in a NonEmptyWindowsStackSet.
    idx <- choose (0, length ws - 1)
    pure $ member (ws !! idx) x


-- preserve order
prop_filter_order :: T -> Bool
prop_filter_order x =
    -- case stack . workspace $ current x of
    case x^._currentStack of
    Nothing -> True
    Just s@(Stack i _ _) -> (integrate' . S.filter (/= i)) s == (filter (/= i) . integrate' . Just) s

-- differentiate should return Nothing if the list is empty or Just stack, with
-- the first element of the list is current, and the rest of the list is down.
prop_differentiate :: [Int] -> Bool
prop_differentiate xs =
    if null xs
    then isNothing (differentiate xs)
    else differentiate xs == Just (Stack (head xs) [] (tail xs))
