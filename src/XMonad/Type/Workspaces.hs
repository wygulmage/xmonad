
module XMonad.Type.Workspaces where

import Control.Lens
import XMonad.Class.HasName
import XMonad.Class.HasRectangle
import XMonad.StackSet (RationalRect (..), Stack (..))
import Data.Map (Map)
import Data.IntSet (IntSet)
import Data.Set (Set)
import Data.Coerce (coerce)
import Graphics.X11.Xlib (Rectangle, Window)


------- Workspaces -------

data Workspaces l = Workspaces
    { current :: Screen l
    , visible :: Set (Screen l)
    , hidden :: Set (Workspace l)
    , floating :: Map Window RationalRect
    } deriving (Show, Read, Eq)


------- Workspace -------

type WorkspaceId = String

data Workspace l = Workspace
    WorkspaceId
    l
    (Maybe (Stack Window))
    deriving (Show, Read)

instance Eq (Workspace l) where
    (==) = (==) `on` view _name

instance HasName (Workspace l) where
    _name f (Workspace name layout ws) =
        (\ name' -> Workspace name' layout ws) <$> f name

_layout :: Lens' (Workspace l) l
_layout f (Workspace name layout ws) =
    (\ layout' -> Workspace name layout' ws) <$> f layout

_WindowStack f (Workspace name layout ws) =
    Workspace name layout <$> f ws


------- Screen -------

type ScreenID = Int

data Screen l = Screen
    ScreenID
    Rectangle
    (Workspace l)
    deriving (Show, Read)

instance Eq (Screen l) where
  (==) = (==) `on` view _screenID

instance Ord (Screen l) where
  compare = compare `on` view _screenID

instance HasRectangle (Screen l) where
    _Rectangle f (Screen name r ws) =
        (\ r' -> Screen name r' ws) <$> f r

_screenID :: Lens' (Screen l) ScreenID
_screenID f (Screen sid r ws) =
    (\ sid' -> Screen sid' r ws) <$> f sid

_workspace :: Lens' (Screen l) (Workspace WorkspaceId l Window)
_workspace f (Screen sid r ws) =
    (Screen sid r) <$> f ws
