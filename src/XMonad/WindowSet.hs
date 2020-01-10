{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XMonad.WindowSet where

import Data.Foldable

import qualified Data.List as List

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import Data.Map (Map)
import qualified Data.Map as Map

-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import XMonad.Zipper (Stack (..), _focus, differentiate, filter, integrate, integrate')
import qualified XMonad.Zipper as Stack

import Graphics.X11.Xlib (Rectangle, Window)

import Lens.Micro
import Lens.Micro.Mtl
import XMonad.Internal.Optic


type ScreenId = Int
type WorkspaceId = String

data WindowSet layout = WindowSet{
    current :: !(Screen layout) -- the screen with active focus
    ,
    currentId :: !ScreenId -- id of the screen with active focus
    ,
    inactive :: !(WindowSetInactive layout)
    }

_current :: Lens' (WindowSet layout) (Screen layout)
_current f s = (\ x -> s{ current = x }) <$> f (current s)

_currentId :: Lens' (WindowSet layout) ScreenId
_currentId f s = (\ x -> s{ currentId = x }) <$> f (currentId s)

_inactive :: Lens' (WindowSet layout) (WindowSetInactive layout)
_inactive f s = (\ x -> s{ inactive = x }) <$> f (inactive s)

_visible :: Lens' (WindowSet layout) (Map ScreenId (Screen layout))
_visible = _inactive . _windowSetInactive_visible

data WindowSetInactive layout = WindowSetInactive{
    visible :: !(Map ScreenId (Screen layout)) -- Workspaces, excluding current, that are shown on screens
    ,
    hidden :: !(Map WorkspaceId (Workspace layout)) -- Workspaces that are not mapped to screens
    ,
    floating :: !(Map Window RealRect) -- Windows that are not tiled (independent of workspace; may overlap multiple screens)
    }

_windowSetInactive_visible ::
    Lens' (WindowSetInactive layout) (Map ScreenId (Screen layout))
_windowSetInactive_visible f s = (\ x -> s{ visible = x }) <$> f (visible s)

emptyInactive :: WindowSetInactive layout
emptyInactive = WindowSetInactive{
    visible = mempty
    ,
    hidden = mempty
    ,
    floating = mempty
    }

data Screen layout = Screen {
    workspace :: !(Workspace layout) -- the workspace shown on the screen
    ,
    workspaceId :: !WorkspaceId
    ,
    rectangle :: !Rectangle -- the dimensions and position of the screen
    }

_workspace :: Lens (Screen layout) (Screen layout') (Workspace layout) (Workspace layout')
_workspace f s = (\ x -> s{ workspace = x }) <$> f (workspace s)

_workspaceId :: Lens' (Screen layout) WorkspaceId
_workspaceId f s = (\ x -> s{ workspaceId = x }) <$> f (workspaceId s)

data Workspace layout = Workspace {
    layout :: !layout
    ,
    zipper :: !(Maybe (Stack Window))
    }

data RealRect = RealRect !Double !Double !Double !Double

newWorkspace :: layout -> Workspace layout
newWorkspace lay = Workspace{ layout = lay, zipper = mempty }

newScreen :: layout -> WorkspaceId -> Rectangle -> Screen layout
newScreen lay wksId rect = Screen{
    workspace = newWorkspace lay
    ,
    workspaceId = wksId
    ,
    rectangle = rect
    }

newWindowSetFromLists ::
    forall layout.
    layout -> NonEmpty WorkspaceId -> NonEmpty Rectangle -> WindowSet layout
-- Given a layout, a nonempty list of WorkspaceIds, and a nonempty list of screen positions, construct a new window set.
newWindowSetFromLists lay (wsId0 :| wsIds) (scrRect0 :| scrRects)
    | wsIds `compareLength` scrRects == GT = WindowSet cur 0 inact
  where
    cur :: Screen layout
    cur = Screen emptyWorkspace wsId0 scrRect0

    inact :: WindowSetInactive layout
    inact = WindowSetInactive{
        visible = vis
        ,
        hidden = Map.fromList (fmap (flip (,) emptyWorkspace) hid)
        ,
        floating = mempty
        }

    (seen, hid) = List.splitAt (length scrRects) wsIds

    vis :: Map ScreenId (Screen layout)
    vis = Map.fromList
        (List.zip [1..] (List.zipWith (Screen emptyWorkspace) seen scrRects))

    emptyWorkspace :: Workspace layout
    emptyWorkspace = Workspace lay Nothing
newWindowSetFromLists _ _ _ = error "newWindowSetFromLists: There are not enough workspace Ids to have at least one workspace per screen."

-- activate :: WorkspaceId -> WindowSet layout -> WindowSet layout
-- activate wkId ws
--     | wkId == view (_current . _workspaceId) ws = ws
--     | Just x <-
--         List.find ((wkId ==) . view _workspaceId) (toListOf _visible ws)
--         =
--           _visible %~ Map.insert (view _currentId ws) (view _current ws) $
--           _current . _workspace .~ x $
--           _current . _workspaceId .~ wkId $
--           _visible %~ Map.delete wkId $
--           ws


compareLength :: [a] -> [b] -> Ordering
compareLength [] [] = EQ
compareLength [] _ = LT
compareLength _ [] = GT
compareLength (_ : xs) (_ : ys) = compareLength xs ys
