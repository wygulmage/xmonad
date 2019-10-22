
module XMonad.Internal.Spaghetti where
--- Interdependent Types and Classes

import Graphics.X11 (Rectangle, Window)
-- import Graphics.X11.Xlib.Extras (Event)
import XMonad.Zipper (Stack)
import XMonad.Internal.Message
import XMonad.Internal.Type.WorkspaceId

data Workspace m = Workspace
    { workspaceId :: WorkspaceId
    , layout :: Layout m
    , windowset :: Maybe (Stack Window)
    }

data Layout m = Layout
    { runLayout :: RunLayout m
    , handleMessage :: HandleMessage m
    , description :: String
    }

type RunLayout m = Workspace m -> Rectangle -> m ([(Window, Rectangle)], Maybe (Layout m))

type HandleMessage m = SomeMessage -> m (Maybe (Layout m))
