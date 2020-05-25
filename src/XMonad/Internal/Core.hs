{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XMonad.Internal.Core where


-- import Prelude
-- import Control.Applicative (Applicative, liftA2, pure, (<$>))
-- import Control.Exception.Extensible
--     (SomeException (..), bracket, finally, fromException, throw, try)
-- import qualified Control.Exception.Extensible as E
-- import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader
import Control.Monad.State (StateT (..))
-- import Data.Default
-- import Data.Foldable (fold, for_)
-- import Data.Functor (($>))
-- import Data.List ((\\))
import Data.Map (Map)
-- import Data.Maybe (fromMaybe, isJust)
import Data.Monoid hiding ((<>))
-- import Data.Semigroup
import Data.Set (Set)
import Data.Typeable
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Types (XPosition, YPosition)
import Graphics.X11.Xlib.Extras
    ( Event
    -- , WindowAttributes, getWindowAttributes
    )
-- import Lens.Micro (Lens, Lens', (%~), (.~))
-- import qualified Lens.Micro.Mtl as Lens
-- import System.Directory
-- import System.Environment (lookupEnv)
-- import System.Exit (ExitCode (..))
-- import System.FilePath
-- import System.Info (arch, os)
-- import System.IO
-- import System.Posix.Env (getEnv)
-- import System.Posix.IO
-- import System.Posix.Process
--     (createSession, executeFile, forkProcess, getAnyProcessStatus)
-- import System.Posix.Signals
-- import System.Posix.Types (ProcessID)
-- import System.Process

import XMonad.Internal.Type.Star (Star (..))
import XMonad.Internal.Type.Zipper
import XMonad.Internal.WindowSet

type X = Star (StateT XState IO) XConf

type WindowSet = StackSet' (Layout Window)

newtype Query a = Query (ReaderT Window X a)
    deriving (Functor, Applicative, Monad, MonadReader Window, MonadIO)

type ManageHook = Query (Endo WindowSet)

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
        , dragging        :: !(Maybe (XPosition -> YPosition -> X (), X ()))
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
        , mousePosition :: !(Maybe (XPosition, YPosition))
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

------------------------------------------------------------------------
-- LayoutClass handling. See particular instances in Operations.hs
-- | An existential type that can hold any object that is in 'Read'
--   and 'LayoutClass'.
data Layout a =
    forall l. (LayoutClass l a, Read (l a)) =>
              Layout (l a)

-- | Using the 'Layout' as a witness, parse existentially wrapped windows
-- from a 'String'.
readsLayout :: Layout a -> String -> [(Layout a, String)]
readsLayout (Layout l) s = [(Layout (asTypeOf x l), rs) | (x, rs) <- reads s]

-- | Every layout must be an instance of 'LayoutClass', which defines
-- the basic layout operations along with a sensible default for each.
--
-- Minimal complete definition:
--
-- * 'runLayout' || (('doLayout' || 'pureLayout') && 'emptyLayout'), and
--
-- * 'handleMessage' || 'pureMessage'
--
-- You should also strongly consider implementing 'description',
-- although it is not required.
--
-- Note that any code which /uses/ 'LayoutClass' methods should only
-- ever call 'runLayout', 'handleMessage', and 'description'!  In
-- other words, the only calls to 'doLayout', 'pureMessage', and other
-- such methods should be from the default implementations of
-- 'runLayout', 'handleMessage', and so on.  This ensures that the
-- proper methods will be used, regardless of the particular methods
-- that any 'LayoutClass' instance chooses to define.
class Show (layout a) =>
      LayoutClass layout a
    -- | By default, 'runLayout' calls 'doLayout' if there are any
    --   windows to be laid out, and 'emptyLayout' otherwise.  Most
    --   instances of 'LayoutClass' probably do not need to implement
    --   'runLayout'; it is only useful for layouts which wish to make
    --   use of more of the 'Workspace' information (for example,
    --   "XMonad.Layout.PerWorkspace").
    where
    runLayout ::
           Workspace WorkspaceId (layout a) a
        -> Rectangle
        -> X ([(a, Rectangle)], Maybe (layout a))
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
           layout a
        -> Rectangle
        -> Stack a
        -> X ([(a, Rectangle)], Maybe (layout a))
    doLayout l r s = pure (pureLayout l r s, Nothing)
    -- | This is a pure version of 'doLayout', for cases where we
    -- don't need access to the 'X' monad to determine how to lay out
    -- the windows, and we don't need to modify the layout itself.
    pureLayout :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
    pureLayout _ r s = [(focus s, r)]
    -- | 'emptyLayout' is called when there are no windows.
    emptyLayout ::
           layout a -> Rectangle -> X ([(a, Rectangle)], Maybe (layout a))
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
    handleMessage :: layout a -> SomeMessage -> X (Maybe (layout a))
    handleMessage l = pure . pureMessage l
    -- | Respond to a message by (possibly) changing our layout, but
    -- taking no other action.  If the layout changes, the screen will
    -- be refreshed.
    pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
    pureMessage _ _ = Nothing
    -- | This should be a human-readable string that is used when
    -- selecting layouts by name.  The default implementation is
    -- 'show', which is in some cases a poor default.
    description :: layout a -> String
    description = show

instance LayoutClass Layout Window where
    runLayout (Workspace i (Layout l) ms) r =
        fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
    doLayout (Layout l) r s = fmap (fmap Layout) `fmap` doLayout l r s
    emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
    handleMessage (Layout l) = fmap (fmap Layout) `fmap` handleMessage l
    description (Layout l) = description l

instance Show (Layout a) where
    show (Layout l) = show l



-- | Based on ideas in /An Extensible Dynamically-Typed Hierarchy of
-- Exceptions/, Simon Marlow, 2006. Use extensible messages to the
-- 'handleMessage' handler.
--
-- User-extensible messages must be a member of this class.
--
class Typeable a =>
      Message a


-- |
-- A wrapped value of some type in the 'Message' class.
--
data SomeMessage =
    forall a. Message a =>
              SomeMessage a

-- |
-- And now, unwrap a given, unknown 'Message' type, performing a (dynamic)
-- type check on the result.
--
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- X Events are valid Messages.
instance Message Event

-- | 'LayoutMessages' are core messages that all layouts (especially stateful
-- layouts) should consider handling.
data LayoutMessages
    = Hide -- ^ sent when a layout becomes non-visible
    | ReleaseResources -- ^ sent when xmonad is exiting or restarting
    deriving (Typeable, Eq)

instance Message LayoutMessages

-- ---------------------------------------------------------------------
-- Extensible state
--
-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a =>
      ExtensionClass a
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
    = forall a. ExtensionClass a =>
                StateExtension a
    -- ^ Non-persistent state extension
    | forall a. (Read a, Show a, ExtensionClass a) =>
                PersistentExtension a -- ^ Persistent extension
