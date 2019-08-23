
module XMonad.Types.Screens where


import Control.Lens
import Graphics.X11.Xlib.Types (Rectangle)
import qualified XMonad.Type.Focused.Map as F
import XMonad.Type.Workspace (Workspace)

type Screens = F.Map ScreenID (Rectangle, Workspace)

type ScreenID = Int
