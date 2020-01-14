{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XMonad.WindowSet where

import Data.Foldable

import qualified Data.List as List

import Data.List.NonEmpty (NonEmpty (..))
-- import qualified Data.List.NonEmpty as NonEmpty

import Data.Map (Map)
import qualified Data.Map as Map

-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import XMonad.Zipper (Stack (..), _focus, differentiate, filter, integrate, integrate')
import qualified XMonad.Zipper as Stack

import Graphics.X11.Xlib (Rectangle, Window)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import XMonad.Internal.Optic


type ScreenId = Int
type WorkspaceId = String

data WindowSet layout = WindowSet{
    screens :: (ScreenMap layout)
    ,
    hidden :: !(Map WorkspaceId (Workspace layout)) -- Workspaces that are not mapped to screens
    ,
    floating :: !(Map Window RealRect) -- Windows that are not tiled (independent of workspace; may overlap multiple screens)
    }

-- _current :: Lens' (WindowSet layout) (Screen layout)
-- _current f s = (\ x -> s{ current = x }) <$> f (current s)

-- _currentId :: Lens' (WindowSet layout) ScreenId
-- _currentId f s = (\ x -> s{ currentId = x }) <$> f (currentId s)

-- _visible :: Lens' (WindowSet layout) (Map ScreenId (Screen layout))
-- _visible f s = (\ x -> s{ visible = x }) <$> f (visible s)

_hidden :: Lens' (WindowSet layout) (Map WorkspaceId (Workspace layout))
_hidden f s = (\ x -> s{ hidden = x }) <$> f (hidden s)

_floating :: Lens' (WindowSet layout) (Map Window RealRect)
_floating f s = (\ x -> s{ floating = x }) <$> f (floating s)


data ScreenMap layout = ScreenMap{
    current :: !(Screen layout) -- the active screen
    ,
    currentId :: !ScreenId -- the active screen's id
    ,
    visible :: !(Map ScreenId (Screen layout)) -- inactive screens
    }

class HasCurrent a where
    _current :: Lens' a (Screen layout)


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

_rectangle :: Lens' (Screen layout) Rectangle
_rectangle f s = (\ x -> s{ rectangle = x }) <$> f (rectangle s)


data Workspace layout = Workspace {
    layout :: !layout
    ,
    zipper :: !(Maybe (Stack Window))
    }

_layout :: Lens (Workspace layout) (Workspace layout') layout layout'
_layout f s = (\ x -> s{ layout = x }) <$> f (layout s)

_zipper :: Lens' (Workspace layout) (Maybe (Stack Window))
_zipper f s = (\ x -> s{ zipper = x }) <$> f (zipper s)

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

-- newWindowSetFromLists ::
--     forall layout.
--     layout -> NonEmpty WorkspaceId -> NonEmpty Rectangle -> WindowSet layout
-- -- Given a layout, a nonempty list of WorkspaceIds, and a nonempty list of screen positions, construct a new window set.
-- newWindowSetFromLists lay (wsId0 :| wsIds) (scrRect0 :| scrRects)
--     | wsIds `compareLength` scrRects == GT = WindowSet cur 0 vis hid mempty
--   where
--     cur :: Screen layout
--     cur = Screen emptyWorkspace wsId0 scrRect0

--     (seen, unseen) = List.splitAt (length scrRects) wsIds

--     vis :: Map ScreenId (Screen layout)
--     vis = Map.fromList
--         (List.zip [1..] (List.zipWith (Screen emptyWorkspace) seen scrRects))

--     hid :: Map WorkspaceId (Workspace layout)
--     hid = Map.fromList (fmap (flip (,) emptyWorkspace) unseen)

--     emptyWorkspace :: Workspace layout
--     emptyWorkspace = newWorkspace lay

-- newWindowSetFromLists _ _ _ = error "newWindowSetFromLists: There are not enough workspace Ids to have at least one workspace per screen."


-- activate :: WorkspaceId -> WindowSet layout -> WindowSet layout
-- activate wkId ws
--     | isCurrentWorkspace wkId = ws
--     | Just x <- findVisibleWorkspace wkId
--         =
--           _visible %~ Map.insert (view _currentId ws) (view _current ws) $
--           _current . _workspace .~ x $
--           _current . _workspaceId .~ wkId $
--           _visible %~ Map.delete wkId $
--           ws
--   where
--     isCurrentWorkspace :: WorkspaceId -> Bool
--     isCurrentWorkspace = (view (_current . _workspaceId) ws ==)

--     findVisibleWorkspace :: WorkspaceId -> Maybe WorkspaceId
--     findVisibleWorkspace wkId' =
--         List.find ((wkId' ==) . view _workspaceId) (toListOf _visible ws)
--     activateVisibleWorkspace :: WorkspaceId -> Maybe (WindowSet)
--     activateVisibleWorkspace wkId' =
--       where
--         kvs :: [(ScreenId, Screen)]
--         kvs = Map.toList (view _visible ws)


compareLength :: [a] -> [b] -> Ordering
compareLength (_ : xs) (_ : ys) = compareLength xs ys
compareLength [] [] = EQ
compareLength [] _ = LT
compareLength _ [] = GT
