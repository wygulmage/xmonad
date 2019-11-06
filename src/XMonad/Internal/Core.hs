module XMonad.Internal.Core where

import XMonad.StackSet hiding (modify)

-- import Prelude
import Control.Applicative (Applicative, liftA2, pure, (<$>))
import Control.Exception.Extensible
    (SomeException (..), bracket, finally, fromException, throw, try)
import qualified Control.Exception.Extensible as E
import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader
import Control.Monad.State (StateT (..))
import Data.Default
import Data.Foldable (fold, for_)
import Data.Functor (($>))
import Data.List ((\\))
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (Event, WindowAttributes, getWindowAttributes)
import Lens.Micro (Lens, Lens', (%~), (.~))
import qualified Lens.Micro.Mtl as Lens
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath
import System.Info (arch, os)
import System.IO
import System.Posix.Env (getEnv)
import System.Posix.IO
import System.Posix.Process
    (createSession, executeFile, forkProcess, getAnyProcessStatus)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process

import XMonad.Internal.Type.Star (Star (..))
import XMonad.Internal.WindowSet

type X = Star (StateT XState IO) (XConf)

data Env = Env !XState !XConf

-- | XState, the (mutable) window manager state.
data XState =
    XState
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

-- | XConf, the (read-only) window manager configuration.
data XConf =
    XConf
        { display       :: Display
    -- ^ the X11 display
        , config        :: !(XConfig Layout)
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

-- TODO: better name
data XConfig l =
    XConfig
        { normalBorderColor :: !String
    -- ^ Nonfocused windows' border color. Default: \"#dddddd\"
        , focusedBorderColor :: !String
    -- ^ Focused windows border color. Default: \"#ff0000\"
        , terminal :: !String
    -- ^ The preferred terminal application. Default: \"xterm\"
        , layoutHook :: !(l Window)
    -- ^ The available layouts
        , manageHook :: !ManageHook
    -- ^ The action to run when a new window is opened
        , handleEventHook :: !(Event -> X All)
    -- ^ Handle an X event, returns (All True) if the default handler should also be run afterwards. mappend should be used for combining event hooks in most cases.
        , workspaces :: ![String]
    -- ^ The list of workspaces' names
        , modMask :: !KeyMask
    -- ^ the mod modifier
        , keys :: !(XConfig Layout -> Map (ButtonMask, KeySym) (X ()))
    -- ^ The key binding: a map from key presses and actions
        , mouseBindings :: !(XConfig Layout -> Map (ButtonMask, Button) (Window -> X ()))
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
        , handleExtraArgs :: !([String] -> XConfig Layout -> IO (XConfig Layout))
    -- ^ Modify the configuration, complain about extra arguments etc. with arguments that are not handled by default
        }
