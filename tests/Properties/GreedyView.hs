{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
module Properties.GreedyView where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

import Data.List (sortBy)

import Control.Lens ((^.))

-- ---------------------------------------------------------------------
-- greedyViewing workspaces

-- greedyView sets the current workspace to 'n'
prop_greedyView_current (x :: T)  = do
    n <- arbitraryTag x
    pure $ (greedyView n x)^._currentTag == n

-- greedyView leaves things unchanged for invalid workspaces
prop_greedyView_current_id (x :: T) = do
  n <- arbitrary `suchThat` \n' -> not $ n' `tagMember` x
  pure $ (greedyView n x)^._currentTag == x^._currentTag

-- greedyView *only* sets the current workspace, and touches Xinerama.
-- no workspace contents will be changed.
prop_greedyView_local  (x :: T) = do
    n <- arbitraryTag x
    pure $ workspaces x == workspaces (greedyView n x)
  where
    workspaces a = sortBy (\s t -> tag s `compare` tag t) $
                                    workspace (current a)
                                    : map workspace (visible a) ++ hidden a

-- greedyView is idempotent
prop_greedyView_idem (x :: T) = do
  n <- arbitraryTag x
  pure $ greedyView n (greedyView n x) == (greedyView n x)

-- greedyView is reversible, though shuffles the order of hidden/visible
prop_greedyView_reversible (x :: T) = do
    n <- arbitraryTag x
    pure $ normal (greedyView n' (greedyView n x)) == normal x
    where n'  = x^._currentTag
