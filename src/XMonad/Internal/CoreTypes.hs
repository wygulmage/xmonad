{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XMonad.Internal.Types where


import Data.Foldable

import qualified Data.List as List

import Data.List.NonEmpty (NonEmpty (..))
-- import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (All (..), Endo (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State

import Graphics.X11.Xlib
    (Button, ButtonMask, Dimension, Display, EventMask, KeyMask, KeySym, Pixel, Position, Rectangle, Window)
import Graphics.X11.Xlib.Extras (Event, WindowAttributes, getWindowAttributes)

import Lens.Micro
import Lens.Micro.Mtl

import XMonad.Zipper (Stack (..), _focus, differentiate, filter, integrate, integrate')
import qualified XMonad.Zipper as Stack
import XMonad.Internal.Optic
import XMonad.Internal.Type.Star
-- import XMonad.Internal.Type.Zipper


type X = Star (StateT XState IO) XConf
type ManageHook = Query (Endo WindowSet)
type Query = Star X Window

type ScreenId = Int
type WorkspaceId = String

data XState = XState
        { windowset       :: !WindowSet
    -- ^ workspace list
        , mapped          :: !(Set Window)
    -- ^ the Set of mapped windows
        , waitingUnmap    :: !(Map Window Int)
    -- ^ the number of expected UnmapEvents
        , dragging        :: !(Maybe (Position -> Position -> X (), X ()))
        , numberlockMask  :: !KeyMask
    -- ^ the numlock modifier
        , extensibleState :: !(Map String (Either String StateExtension))
    -- ^ stores custom state information.
    -- The module "XMonad.Util.ExtensibleState" in xmonad-contrib
    -- provides additional information and a simple interface for using this.
        }

data XConf = XConf
        { display       :: Display
    -- ^ the X11 display
        , config        :: !XConfig
    -- ^ initial user configuration
        , theRoot       :: !Window
    -- ^ the root window
        , normalBorder  :: !Pixel
    -- ^ border color of unfocused windows
        , focusedBorder :: !Pixel
    -- ^ border color of the focused window
        , keyActions    :: !(Map (KeyMask, KeySym) (X ()))
    -- ^ a mapping of key presses to actions
        , buttonActions :: !(Map (KeyMask, Button) (Window -> X ()))
    -- ^ a mapping of button presses to actions
        , mouseFocused  :: !Bool
    -- ^ was refocus caused by mouse action?
        , mousePosition :: !(Maybe (Position, Position))
    -- ^ position of the mouse according to the event currently being processed
        , currentEvent  :: !(Maybe Event)
    -- ^ event currently being processed
        }


data XConfig =
    XConfig
        { normalBorderColor :: !String
    -- ^ Nonfocused windows' border color. Default: \"#dddddd\"
        , focusedBorderColor :: !String
    -- ^ Focused windows border color. Default: \"#ff0000\"
        , terminal :: !String
    -- ^ The preferred terminal application. Default: \"xterm\"
        , layoutHook :: !(forall l. Layout l => l)
    -- ^ The available layouts
        , manageHook :: !ManageHook
    -- ^ The action to run when a new window is opened
        , handleEventHook :: !(Event -> X All)
    -- ^ Handle an X event, returns (All True) if the default handler should also be run afterwards. mappend should be used for combining event hooks in most cases.
        , workspaces :: ![String]
    -- ^ The list of workspaces' names
        , modMask :: !KeyMask
    -- ^ the mod modifier
        , keys :: !(XConfig -> Map (ButtonMask, KeySym) (X ()))
    -- ^ The key binding: a map from key presses and actions
        , mouseBindings :: !(XConfig -> Map (ButtonMask, Button) (Window -> X ()))
    -- ^ The mouse bindings
        , borderWidth :: !Dimension
    -- ^ The border width
        , logHook :: !(X ())
    -- ^ The action to perform when the windows set is changed
        , startupHook :: !(X ())
    -- ^ The action to perform on startup
        , focusFollowsMouse :: !Bool
    -- ^ Whether window entry events can change focus
        , clickJustFocuses :: !Bool
    -- ^ False to make a click which changes focus to be additionally passed to the window
        , clientMask :: !EventMask
    -- ^ The client events that xmonad is interested in
        , rootMask :: !EventMask
    -- ^ The root events that xmonad is interested in
        , handleExtraArgs :: !([String] -> XConfig -> IO XConfig)
    -- ^ Modify the configuration, complain about extra arguments etc. with arguments that are not handled by default
        }

class Show layout =>
      LayoutClass layout
    -- | By default, 'runLayout' calls 'doLayout' if there are any
    --   windows to be laid out, and 'emptyLayout' otherwise.  Most
    --   instances of 'LayoutClass' probably do not need to implement
    --   'runLayout'; it is only useful for layouts which wish to make
    --   use of more of the 'Workspace' information (for example,
    --   "XMonad.Layout.PerWorkspace").
    where
    runLayout ::
        Workspace -> Rectangle ->
        X ([(Window, Rectangle)], Maybe layout)
    runLayout (Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms
    -- | Given a 'Rectangle' in which to place the windows, and a 'Stack'
    -- of windows, return a list of windows and their corresponding
    -- Rectangles.  If an element is not given a Rectangle by
    -- 'doLayout', then it is not shown on screen.  The order of
    -- windows in this list should be the desired stacking order.
    --
    -- Also possibly return a modified layout (by returning @Just
    -- newLayout@), if this layout needs to be modified (e.g. if it
    -- keeps track of some sort of state).  Return @Nothing@ if the
    -- layout does not need to be modified.
    --
    -- Layouts which do not need access to the 'X' monad ('IO', window
    -- manager state, or configuration) and do not keep track of their
    -- own state should implement 'pureLayout' instead of 'doLayout'.
    doLayout ::
        layout -> Rectangle -> Stack a ->
        X ([(a, Rectangle)], Maybe layout)
    doLayout l r s = pure (pureLayout l r s, Nothing)
    -- | This is a pure version of 'doLayout', for cases where we
    -- don't need access to the 'X' monad to determine how to lay out
    -- the windows, and we don't need to modify the layout itself.
    pureLayout :: layout -> Rectangle -> Stack a -> [(a, Rectangle)]
    pureLayout _ r s = [(focus s, r)]
    -- | 'emptyLayout' is called when there are no windows.
    emptyLayout ::
           layout -> Rectangle -> X ([(Window, Rectangle)], Maybe layout)
    emptyLayout _ _ = pure ([], Nothing)
    -- | 'handleMessage' performs message handling.  If
    -- 'handleMessage' returns @Nothing@, then the layout did not
    -- respond to the message and the screen is not refreshed.
    -- Otherwise, 'handleMessage' returns an updated layout and the
    -- screen is refreshed.
    --
    -- Layouts which do not need access to the 'X' monad to decide how
    -- to handle messages should implement 'pureMessage' instead of
    -- 'handleMessage' (this restricts the risk of error, and makes
    -- testing much easier).
    handleMessage :: layout -> SomeMessage -> X (Maybe layout)
    handleMessage l = pure . pureMessage l
    -- | Respond to a message by (possibly) changing our layout, but
    -- taking no other action.  If the layout changes, the screen will
    -- be refreshed.
    pureMessage :: layout -> SomeMessage -> Maybe layout
    pureMessage _ _ = Nothing
    -- | This should be a human-readable string that is used when
    -- selecting layouts by name.  The default implementation is
    -- 'show', which is in some cases a poor default.
    description :: layout -> String
    description = show

data Layout = forall l. (LayoutClass l, Read l) => Layout l
instance LayoutClass Layout where
    runLayout (Layout l) = runLayout l
    doLayout (Layout l) = doLayout l
    pureLayout (Layout l) = pureLayout l
    emptyLayout (Layout l) = emptyLayout l
    handleMessage (Layout l) = handleMessage l
    pureMessage (Layout l) = pureMessage l
    description (Layout l) = description l


class Typeable a => Message a

data SomeMessage = forall a. Message a => SomeMessage a


class Typeable a => ExtensionClass a
    -- | Defines an initial value for the state extension
    where
    initialValue :: a
    -- | Specifies whether the state extension should be
    -- persistent. Setting this method to 'PersistentExtension'
    -- will make the stored data survive restarts, but
    -- requires a to be an instance of Read and Show.
    --
    -- It defaults to 'StateExtension', i.e. no persistence.
    extensionType :: a -> StateExtension
    extensionType = StateExtension

-- | Existential type to store a state extension.
data StateExtension
    = forall a. ExtensionClass a => StateExtension a
    -- ^ Non-persistent state extension
    | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a -- ^ Persistent extension


data WindowSet = WindowSet{
    current :: !(Screen) -- the screen with active focus
    ,
    currentId :: !ScreenId -- id of the screen with active focus
    ,
    visible :: !(Map ScreenId (Screen)) -- Workspaces, excluding current, that are shown on screens
    ,
    hidden :: !(Map WorkspaceId (Workspace)) -- Workspaces that are not mapped to screens
    ,
    floating :: !(Map Window RealRect) -- Windows that are not tiled (independent of workspace; may overlap multiple screens)
    }

_current :: Lens' WindowSet (Screen Layout)
_current f s = (\ x -> s{ current = x }) <$> f (current s)

_currentId :: Lens' WindowSet ScreenId
_currentId f s = (\ x -> s{ currentId = x }) <$> f (currentId s)

_visible :: Lens' WindowSet (Map ScreenId (Screen layout))
_visible f s = (\ x -> s{ visible = x }) <$> f (visible s)

_hidden :: Lens' WindowSet (Map WorkspaceId (Workspace layout))
_hidden f s = (\ x -> s{ hidden = x }) <$> f (hidden s)

_floating :: Lens' WindowSet (Map Window RealRect)
_floating f s = (\ x -> s{ floating = x }) <$> f (floating s)

data Screen = Screen {
    workspace :: !Workspace -- the workspace shown on the screen
    ,
    workspaceId :: !WorkspaceId
    ,
    rectangle :: !Rectangle -- the dimensions and position of the screen
    }

_workspace :: Lens Screen Screen Workspace Workspace
_workspace f s = (\ x -> s{ workspace = x }) <$> f (workspace s)

_workspaceId :: Lens' Screen WorkspaceId
_workspaceId f s = (\ x -> s{ workspaceId = x }) <$> f (workspaceId s)

_rectangle :: Lens' Screen Rectangle
_rectangle f s = (\ x -> s{ rectangle = x }) <$> f (rectangle s)

data Workspace = Workspace {
    layout :: !(forall l. LayoutClass l => l)
    ,
    zipper :: !(Maybe (Stack Window))
    }

_layout :: Lens Workspace Workspace Layout Layout
_layout f s = (\ x -> s{ layout = x }) <$> f (layout s)

_zipper :: Lens' Workspace (Maybe (Stack Window))
_zipper f s = (\ x -> s{ zipper = x }) <$> f (zipper s)

data RealRect = RealRect !Double !Double !Double !Double

-- newWorkspace :: layout -> Workspace layout
-- newWorkspace lay = Workspace{ layout = lay, zipper = mempty }

-- newScreen :: layout -> WorkspaceId -> Rectangle -> Screen layout
-- newScreen lay wksId rect = Screen{
--     workspace = newWorkspace lay
--     ,
--     workspaceId = wksId
--     ,
--     rectangle = rect
--     }
