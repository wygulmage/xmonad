{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
module Properties.View where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

import Data.List (sortBy)

import Control.Lens ((^.))
import qualified Control.Lens as Lens

-- ---------------------------------------------------------------------
-- viewing workspaces

-- view sets the current workspace to 'n'
prop_view_current (x :: T) = do
    n <- arbitraryTag x
    -- pure $ (tag . workspace . current . view n) x == n
    pure $ (^. Lens.to (view n) . _currentTag) x == n

-- view *only* sets the current workspace, and touches Xinerama.
-- no workspace contents will be changed.
prop_view_local  (x :: T) = do
    n <- arbitraryTag x
    pure $ workspaces x == workspaces (view n x)
  where
    workspaces a = sortBy (\s t -> tag s `compare` tag t) $
                                    workspace (current a)
                                    : fmap workspace (visible a) <> hidden a

-- TODO: Fix this
-- view should result in a visible xinerama screen
-- prop_view_xinerama (x :: T) (n :: NonNegative Int) = i `tagMember` x ==>
--     M.member i (screens (view i x))
--   where
--     i = fromIntegral n

-- view is idempotent
prop_view_idem (x :: T) = do
    n <- arbitraryTag x
    pure $ view n (view n x) == view n x

-- view is reversible, though shuffles the order of hidden/visible
prop_view_reversible (x :: T) = do
    n <- arbitraryTag x
    pure $ normal (view n' (view n x)) == normal x
  where
  n' = x^._currentTag
