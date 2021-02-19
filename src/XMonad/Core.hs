{-# LANGUAGE ExistentialQuantification
           , FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , DeriveDataTypeable
           , LambdaCase
           , NamedFieldPuns
           , DeriveTraversable
           , ScopedTypeVariables
           , TemplateHaskell
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Core
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, uses cunning newtype deriving
--
-- The 'X' monad, a state monad transformer over 'IO', for the window
-- manager state, and support routines.
--
-----------------------------------------------------------------------------

module XMonad.Core
-- X Monad:
    ( X, runX, catchX, whenX, userCode, userCodeDef
-- Layouts, Workspaces and Configurations:
    , XConf(..), XConfig(..)
    , LayoutClass(..)
    , Layout(..), readsLayout, Typeable, Message
    , ScreenId(..), ScreenDetail(..), XState(..)
    , SomeMessage(..), fromMessage, LayoutMessages(..)
    , StateExtension(..), ExtensionClass(..)
    , ManageHook, Query(..), runQuery
    , WindowSet, WindowSpace, WorkspaceId
    , withDisplay, withWindowSet, isRoot, runOnWorkspaces, withWindowAttributes
-- Directories:
    , Directories(..), Dirs, getDirs
    , getXMonadDir, getXMonadCacheDir, getXMonadDataDir, stateFileName
-- IO and Utilities:
    , io, catchIO, installSignalHandlers, uninstallSignalHandlers
    , spawn, spawnPID, xfork, recompile, trace
    , getAtom, atom_WM_STATE, atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW, atom_WM_TAKE_FOCUS
    , whenJust
-- Lenses
    -- XState
    , _windowset, _mapped, _waitingUnmap, _dragging, _numberlockMask, _extensibleState
    -- XConf
    , _display, _config, _theRoot, _normalBorder, _focusedBorder, _keyActions, _buttonActions, _mouseFocused, _mousePosition, _currentEvent
    -- XConfig
    , _borderWidth, _normalBorderColor, _focusedBorderColor, _terminal
    , _layoutHook, _manageHook, _handleEventHook, _logHook, _startupHook
    , _workspaceNames, _modMask, _keys, _mouseBindings, _focusFollowsMouse, _clickJustFocuses, _clientMask, _rootMask, _handleExtraArgs
    , _screenRect
    ) where

import XMonad.StackSet hiding (modify, workspaces)
import Control.Lens hiding (mapped, view)
import qualified Control.Lens as Lens

import Prelude
import Control.Exception (fromException, try, bracket, throw, finally, SomeException(..))
import qualified Control.Exception as E
import Control.Applicative (Applicative, pure, liftA2, (<*>), (<|>), empty)
import Control.Monad.Fail
import Control.Monad.State
import Control.Monad.Reader
import Data.Semigroup
import Data.Traversable (for)
import Data.Foldable (Foldable, foldMap, fold, traverse_, for_)
import Data.Functor (($>))
import Data.Default
import System.FilePath
import System.IO
import System.Info
import System.Posix.Env (getEnv)
import System.Posix.Process (executeFile, forkProcess, getAnyProcessStatus, createSession)
import System.Posix.Signals
import System.Posix.IO
import System.Posix.Types (ProcessID)
import System.Process
import System.Directory
import System.Exit
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getWindowAttributes, WindowAttributes, Event)
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Typeable
import System.Environment (lookupEnv)
import Path (Path)
import qualified Path
import qualified XMonad.WindowSet as WS


-- | XState, the (mutable) window manager state.
data XState = XState
    { windowset        :: !WindowSet                     -- ^ workspace list
    , mapped           :: !WS.WindowSet                -- ^ the Set of mapped windows
    , waitingUnmap     :: !(Map Window Int)            -- ^ the number of expected UnmapEvents
    , dragging         :: !(Maybe (Position -> Position -> X (), X ()))
    , numberlockMask   :: !KeyMask                       -- ^ The numlock modifier
    , extensibleState  :: !(Map String (Either String StateExtension))
    -- ^ stores custom state information.
    --
    -- The module "XMonad.Util.ExtensibleState" in xmonad-contrib
    -- provides additional information and a simple interface for using this.
    }

_windowset :: Lens' XState WindowSet
_windowset = lens windowset (\ s x -> s{ windowset = x })

_mapped :: Lens' XState WS.WindowSet
_mapped = lens mapped (\ s x -> s{ mapped = x})

_waitingUnmap :: Lens' XState (Map Window Int)
_waitingUnmap = lens waitingUnmap (\ s x -> s{ waitingUnmap = x })

_dragging :: Lens' XState (Maybe (Position -> Position -> X (), X ()))
_dragging = lens dragging (\ s x -> s{ dragging = x })

_numberlockMask :: Lens' XState KeyMask
_numberlockMask = lens numberlockMask (\ s x -> s{ numberlockMask = x })

_extensibleState :: Lens' XState (Map String (Either String StateExtension))
_extensibleState = lens extensibleState (\ s x -> s{ extensibleState = x })


-- | XConf, the (read-only) window manager configuration.
data XConf = XConf
    { display       :: Display        -- ^ the X11 display
    , config        :: !(XConfig Layout)       -- ^ initial user configuration
    , theRoot       :: !Window        -- ^ the root window
    , normalBorder  :: !Pixel         -- ^ border color of unfocused windows
    , focusedBorder :: !Pixel         -- ^ border color of the focused window
    , keyActions    :: !(Map (KeyMask, KeySym) (X ()))
                                      -- ^ a mapping of key presses to actions
    , buttonActions :: !(Map (KeyMask, Button) (Window -> X ()))
                                      -- ^ a mapping of button presses to actions
    , mouseFocused :: !Bool           -- ^ was refocus caused by mouse action?
    , mousePosition :: !(Maybe (Position, Position))
                                      -- ^ position of the mouse according to
                                      -- the event currently being processed
    , currentEvent :: !(Maybe Event)  -- ^ event currently being processed
    , dirs         :: !Dirs           -- ^ directories to use
    }

_display :: Lens' XConf Display
_display = lens display (\ s x -> s{ display = x })

_config :: Lens' XConf (XConfig Layout)
_config = lens config (\ s x -> s{ config = x })

_theRoot :: Lens' XConf Window
_theRoot = lens theRoot (\ s x -> s{ theRoot = x })

_normalBorder :: Lens' XConf Pixel
_normalBorder = lens normalBorder (\ s x -> s{ normalBorder = x })

_focusedBorder :: Lens' XConf Pixel
_focusedBorder = lens focusedBorder (\ s x -> s{ focusedBorder = x })

_keyActions :: Lens' XConf (Map (KeyMask, KeySym) (X ()))
_keyActions = lens keyActions (\ s x -> s{ keyActions = x })

_buttonActions :: Lens' XConf (Map (KeyMask, Button) (Window -> X ()))
_buttonActions = lens buttonActions (\ s x -> s{ buttonActions = x })

_mouseFocused :: Lens' XConf Bool
_mouseFocused = lens mouseFocused (\ s x -> s{ mouseFocused = x })

_mousePosition :: Lens' XConf (Maybe (Position, Position))
_mousePosition = lens mousePosition (\ s x -> s{ mousePosition = x })

_currentEvent :: Lens' XConf (Maybe Event)
_currentEvent f s = (\ x -> s{ currentEvent = x }) <$> f (currentEvent s)



-- todo, better name
data XConfig l = XConfig
    { normalBorderColor  :: !String              -- ^ Non focused windows border color. Default: \"#dddddd\"
    , focusedBorderColor :: !String              -- ^ Focused windows border color. Default: \"#ff0000\"
    , terminal           :: !String              -- ^ The preferred terminal application. Default: \"xterm\"
    , layoutHook         :: !(l Window)          -- ^ The available layouts
    , manageHook         :: !ManageHook          -- ^ The action to run when a new window is opened
    , handleEventHook    :: !(Event -> X All)    -- ^ Handle an X event, returns (All True) if the default handler
                                                 -- should also be run afterwards. mappend should be used for combining
                                                 -- event hooks in most cases.
    , workspaces         :: ![String]            -- ^ The list of workspaces' names
    , modMask            :: !KeyMask             -- ^ the mod modifier
    , keys               :: !(XConfig Layout -> Map (ButtonMask,KeySym) (X ()))
                                                 -- ^ The key binding: a map from key presses and actions
    , mouseBindings      :: !(XConfig Layout -> Map (ButtonMask, Button) (Window -> X ()))
                                                 -- ^ The mouse bindings
    , borderWidth        :: !Dimension           -- ^ The border width
    , logHook            :: !(X ())              -- ^ The action to perform when the windows set is changed
    , startupHook        :: !(X ())              -- ^ The action to perform on startup
    , focusFollowsMouse  :: !Bool                -- ^ Whether window entry events can change focus
    , clickJustFocuses   :: !Bool                -- ^ False to make a click which changes focus to be additionally passed to the window
    , clientMask         :: !EventMask           -- ^ The client events that xmonad is interested in
    , rootMask           :: !EventMask           -- ^ The root events that xmonad is interested in
    , handleExtraArgs    :: !([String] -> XConfig Layout -> IO (XConfig Layout))
                                                 -- ^ Modify the configuration, complain about extra arguments etc. with arguments that are not handled by default
    }


_borderWidth :: Lens' (XConfig l) Dimension
_borderWidth = lens borderWidth (\ s x -> s{ borderWidth = x })

_normalBorderColor, _focusedBorderColor, _terminal :: Lens' (XConfig l) String
_normalBorderColor = lens normalBorderColor (\ s x -> s{ normalBorderColor = x })
_focusedBorderColor = lens focusedBorderColor (\ s x ->s { focusedBorderColor = x })
_terminal = lens terminal (\ s x -> s{ terminal = x })

_layoutHook :: Lens (XConfig l) (XConfig l') (l Window) (l' Window)
_layoutHook = lens layoutHook (\ s x -> s{ layoutHook = x })

_manageHook :: Lens' (XConfig l) ManageHook
_manageHook = lens manageHook (\ s  x -> s{ manageHook = x })

_handleEventHook :: Lens' (XConfig l) (Event -> X All)
_handleEventHook = lens handleEventHook (\ s x -> s{ handleEventHook = x })

_logHook :: Lens' (XConfig l) (X ())
_logHook = lens logHook (\ s x -> s{ logHook = x })

_startupHook :: Lens' (XConfig l) (X ())
_startupHook = lens startupHook (\ s x -> s{ startupHook = x })

_workspaceNames :: Lens' (XConfig l) [String]
_workspaceNames f xconfig@XConfig{ workspaces = x } =
    (\ x' -> xconfig{ workspaces = x' }) <$> f x

_modMask :: Lens' (XConfig l) KeyMask
_modMask = lens modMask (\ s x -> s{ modMask = x })

_keys :: Lens' (XConfig l) (XConfig Layout -> Map (ButtonMask, KeySym) (X ()))
_keys = lens keys (\ s x -> s{ keys = x })

_mouseBindings :: Lens' (XConfig l) (XConfig Layout -> Map (ButtonMask, Button) (Window -> X ()))
_mouseBindings = lens mouseBindings (\ s x -> s{ mouseBindings = x })

_focusFollowsMouse, _clickJustFocuses :: Lens' (XConfig l) Bool
_focusFollowsMouse = lens focusFollowsMouse (\ s x -> s{ focusFollowsMouse = x })
_clickJustFocuses = lens clickJustFocuses (\ s x -> s{ clickJustFocuses = x })

_clientMask :: Lens' (XConfig l) EventMask
_clientMask = lens clientMask (\ s x -> s{ clientMask = x })

_rootMask :: Lens' (XConfig l) EventMask
_rootMask = lens rootMask (\ s x -> s{ rootMask = x })

_handleExtraArgs :: Lens' (XConfig l) ([String] -> XConfig Layout -> IO (XConfig Layout))
_handleExtraArgs = lens handleExtraArgs (\ s x -> s{ handleExtraArgs = x })


type WindowSet = StackSet  WorkspaceId (Layout Window) Window ScreenId ScreenDetail
type WindowSpace = Workspace WorkspaceId (Layout Window) Window

-- | Virtual workspace indices
type WorkspaceId = String

-- | Physical screen indices
newtype ScreenId = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | The 'Rectangle' with screen dimensions
newtype ScreenDetail = SD { screenRect :: Rectangle }
    deriving (Eq,Show, Read)

_screenRect :: Lens' ScreenDetail Rectangle
_screenRect = lens screenRect (\ s x -> s{ screenRect = x })

------------------------------------------------------------------------

-- | The X monad, 'ReaderT' and 'StateT' transformers over 'IO'
-- encapsulating the window manager configuration and state,
-- respectively.
--
-- Dynamic components may be retrieved with 'get', static components
-- with 'ask'. With newtype deriving we get readers and state monads
-- instantiated on 'XConf' and 'XState' automatically.
--
newtype X a = X (ReaderT XConf (StateT XState IO) a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState XState, MonadReader XConf, Typeable)

-- instance Applicative X where
    -- pure = return
    -- (<*>) = ap

instance Semigroup a => Semigroup (X a) where
    (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (X a) where
    mempty = pure mempty
    mappend = (<>)

instance Default a => Default (X a) where
    def = pure def

type ManageHook = Query (Endo WindowSet)
newtype Query a = Query (ReaderT Window X a)
    deriving (Functor, Applicative, Monad, MonadReader Window, MonadIO)

runQuery :: Query a -> Window -> X a
runQuery (Query m) = runReaderT m

instance Semigroup a => Semigroup (Query a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Query a) where
    mempty  = pure mempty
    mappend = (<>)

instance Default a => Default (Query a) where
    def = pure def

-- | Run the 'X' monad, given a chunk of 'X' monad code, and an initial state
-- Return the result, and final state
runX :: XConf -> XState -> X a -> IO (a, XState)
runX c st (X a) = runStateT (runReaderT a c) st

-- | Run in the 'X' monad, and in case of exception, and catch it and log it
-- to stderr, and run the error case.
catchX :: X a -> X a -> X a
catchX job errcase = do
    st <- get
    c <- ask
    -- Why are we using fromException here only to throw it again?
    (a, s') <- io $ runX c st job `E.catch` \ e -> case fromException e of
                        Just (_ :: ExitCode) -> throw e
                        _ -> hPrint stderr e *> runX c st errcase

    put s'
    pure a

-- | Execute the argument, catching all exceptions.  Either this function or
-- 'catchX' should be used at all callsites of user customized code.
userCode :: X a -> X (Maybe a)
-- userCode a = catchX (Just <$> a) (pure Nothing)
userCode = userCodeDef empty . fmap pure

-- | Same as userCode but with a default argument to return instead of using
-- Maybe, provided for convenience.
userCodeDef :: a -> X a -> X a
-- userCodeDef defValue a = fromMaybe defValue <$> userCode a
userCodeDef = flip catchX . pure

-- ---------------------------------------------------------------------
-- Convenient wrappers to state

-- | Run a monad action with the current display settings
withDisplay :: (Display -> X a) -> X a
withDisplay = (=<< Lens.view _display)

-- | Run a monadic action with the current stack set
withWindowSet :: (WindowSet -> X a) -> X a
withWindowSet = (=<< use _windowset)

-- | Safely access window attributes.
withWindowAttributes :: Display -> Window -> (WindowAttributes -> X ()) -> X ()
withWindowAttributes dpy win f = userCodeDef () . traverse_ f =<< userCode (io $ getWindowAttributes dpy win)
-- withWindowAttributes dpy win f = do
--     wa <- userCode (io $ getWindowAttributes dpy win)
--     -- catchX (whenJust wa f) (pure ())
--     userCodeDef () (traverse_ f wa)

-- | True if the given window is the root window
isRoot :: Window -> X Bool
-- isRoot w = (w ==) <$> Lens.view _theRoot
isRoot w = views _theRoot (w ==)

-- | Wrapper for the common case of atom internment
getAtom :: String -> X Atom
getAtom str = withDisplay $ \dpy -> io $ internAtom dpy str False

-- | Common non-predefined atoms
atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW, atom_WM_STATE, atom_WM_TAKE_FOCUS :: X Atom
atom_WM_PROTOCOLS       = getAtom "WM_PROTOCOLS"
atom_WM_DELETE_WINDOW   = getAtom "WM_DELETE_WINDOW"
atom_WM_STATE           = getAtom "WM_STATE"
atom_WM_TAKE_FOCUS      = getAtom "WM_TAKE_FOCUS"

------------------------------------------------------------------------
-- LayoutClass handling. See particular instances in Operations.hs

-- | An existential type that can hold any object that is in 'Read'
--   and 'LayoutClass'.
data Layout a = forall l. (LayoutClass l a, Read (l a)) => Layout (l a)

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
class (Show (layout a), Typeable layout) => LayoutClass layout a where

    -- | By default, 'runLayout' calls 'doLayout' if there are any
    --   windows to be laid out, and 'emptyLayout' otherwise.  Most
    --   instances of 'LayoutClass' probably do not need to implement
    --   'runLayout'; it is only useful for layouts which wish to make
    --   use of more of the 'Workspace' information (for example,
    --   "XMonad.Layout.PerWorkspace").
    runLayout :: Workspace WorkspaceId (layout a) a
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
    doLayout    :: layout a -> Rectangle -> Stack a
                -> X ([(a, Rectangle)], Maybe (layout a))
    doLayout l r s   = pure (pureLayout l r s, Nothing)

    -- | This is a pure version of 'doLayout', for cases where we
    -- don't need access to the 'X' monad to determine how to lay out
    -- the windows, and we don't need to modify the layout itself.
    pureLayout  :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
    pureLayout _ r s = [(focus s, r)]

    -- | 'emptyLayout' is called when there are no windows.
    emptyLayout :: layout a -> Rectangle -> X ([(a, Rectangle)], Maybe (layout a))
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
    runLayout (Workspace i (Layout l) ms) r = fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
    doLayout (Layout l) r s  = fmap (fmap Layout) `fmap` doLayout l r s
    emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
    handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
    description (Layout l)   = description l

instance Show (Layout a) where show (Layout l) = show l

-- | Based on ideas in /An Extensible Dynamically-Typed Hierarchy of
-- Exceptions/, Simon Marlow, 2006. Use extensible messages to the
-- 'handleMessage' handler.
--
-- User-extensible messages must be a member of this class.
--
class Typeable a => Message a

-- |
-- A wrapped value of some type in the 'Message' class.
--
data SomeMessage = forall a. Message a => SomeMessage a

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
data LayoutMessages = Hide              -- ^ sent when a layout becomes non-visible
                    | ReleaseResources  -- ^ sent when xmonad is exiting or restarting
    deriving (Typeable, Eq)

instance Message LayoutMessages

-- ---------------------------------------------------------------------
-- Extensible state
--

-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a => ExtensionClass a where
    -- | Defines an initial value for the state extension
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
  | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a
    -- ^ Persistent extension

-- ---------------------------------------------------------------------
-- | General utilities
--
-- Lift an 'IO' action into the 'X' monad
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Lift an 'IO' action into the 'X' monad.  If the action results in an 'IO'
-- exception, log the exception to stderr and continue normal execution.
catchIO :: MonadIO m => IO () -> m ()
catchIO f = io (f `E.catch` \(SomeException e) -> hPrint stderr e *> hFlush stderr)

-- | spawn. Launch an external application. Specifically, it double-forks and
-- runs the 'String' you pass as a command to \/bin\/sh.
--
-- Note this function assumes your locale uses utf8.
spawn :: MonadIO m => String -> m ()
spawn x = () <$ spawnPID x

-- | Like 'spawn', but returns the 'ProcessID' of the launched application
spawnPID :: MonadIO m => String -> m ProcessID
spawnPID x = xfork $ executeFile "/bin/sh" False ["-c", x] Nothing

-- | A replacement for 'forkProcess' which resets default signal handlers.
xfork :: MonadIO m => IO () -> m ProcessID
xfork x = io . forkProcess . finally nullStdin $ uninstallSignalHandlers *> createSession *> x
    where
    nullStdin = do
        fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
        dupTo fd stdInput
        closeFd fd

-- | This is basically a map function, running a function in the 'X' monad on
-- each workspace with the output of that function being the modified workspace.
runOnWorkspaces :: (WindowSpace -> X WindowSpace) -> X ()
-- runOnWorkspaces job = uses _windowset (_workspaces %= job)
runOnWorkspaces job = do
    ws <- use _windowset
    h <- traverse job $ hidden ws
    c : v <- traverse (_workspace job) $ current ws : visible ws
    -- modify $ \s -> s { windowset = ws { current = c, visible = v, hidden = h } }
    _windowset %= (_current .~ c) . (_visible .~ v) . (_hidden .~ h)

-- | All the directories that xmonad will use.  They will be used for
-- the following purposes:
--
-- * @dataDir@: This directory is used by XMonad to store data files
-- such as the run-time state file and the configuration binary
-- generated by GHC.
--
-- * @cfgDir@: This directory is where user configuration files are
-- stored (e.g, the xmonad.hs file).  You may also create a @lib@
-- subdirectory in the configuration directory and the default recompile
-- command will add it to the GHC include path.
--
-- * @cacheDir@: This directory is used to store temporary files that
-- can easily be recreated.  For example, the XPrompt history file.
--
-- For how these directories are chosen, see 'getDirs'.
--
data Directories a = Dirs
    { dataDir  :: !a
    , cfgDir   :: !a
    , cacheDir :: !a
    }
    deriving (Show, Functor, Foldable, Traversable)

-- | Convenient type alias for the most common case in which one might
-- want to use the 'Directories' type.
type Dirs = Directories FilePath

-- | Build up the 'Dirs' that xmonad will use.  They are chosen as
-- follows:
--
-- 1. If all three of xmonad's environment variables (@XMONAD_DATA_DIR@,
--    @XMONAD_CONFIG_DIR@, and @XMONAD_CACHE_DIR@) are set, use them.
-- 2. If there is a build script called @build@ or configuration
--    @xmonad.hs@ in @~\/.xmonad@, set all three directories to
--    @~\/.xmonad@.
-- 3. Otherwise, use the @xmonad@ directory in @XDG_DATA_HOME@,
--    @XDG_CONFIG_HOME@, and @XDG_CACHE_HOME@ (or their respective
--    fallbacks).  These directories are created if necessary.
--
-- The xmonad configuration file (or the build script, if present) is
-- always assumed to be in @cfgDir@.
--
getDirs :: IO Dirs
getDirs = xmEnvDirs <|> xmDirs <|> xdgDirs
  where
    -- | Check for xmonad's environment variables first
    xmEnvDirs :: IO Dirs
    xmEnvDirs = do
        let xmEnvs = Dirs{ dataDir  = "XMONAD_DATA_DIR"
                         , cfgDir   = "XMONAD_CONFIG_DIR"
                         , cacheDir = "XMONAD_CACHE_DIR"
                         }
        maybe empty pure . sequenceA =<< traverse getEnv xmEnvs

    -- | Check whether the config file or a build script is in the
    -- @~\/.xmonad@ directory
    xmDirs :: IO Dirs
    xmDirs = do
        xmDir <- getAppUserDataDirectory "xmonad"
        conf  <- doesFileExist $ xmDir </> "xmonad.hs"
        build <- doesFileExist $ xmDir </> "build"

        -- Place *everything* in ~/.xmonad if yes
        guard $ conf || build
        pure Dirs{ dataDir = xmDir, cfgDir = xmDir, cacheDir = xmDir }

    -- | Use XDG directories as a fallback
    xdgDirs :: IO Dirs
    xdgDirs =
        for Dirs{ dataDir = XdgData, cfgDir = XdgConfig, cacheDir = XdgCache }
            $ \dir -> do d <- getXdgDirectory dir "xmonad"
                         d <$ createDirectoryIfMissing True d

-- | Return the path to the xmonad configuration directory.
getXMonadDir :: X String
getXMonadDir = asks (cfgDir . dirs)
{-# DEPRECATED getXMonadDir "Use `asks (cfgDir . dirs)' instead." #-}

-- | Return the path to the xmonad cache directory.
getXMonadCacheDir :: X String
getXMonadCacheDir = asks (cacheDir . dirs)
{-# DEPRECATED getXMonadCacheDir "Use `asks (cacheDir . dirs)' instead." #-}

-- | Return the path to the xmonad data directory.
getXMonadDataDir :: X String
getXMonadDataDir = asks (dataDir . dirs)
{-# DEPRECATED getXMonadDataDir "Use `asks (dataDir . dirs)' instead." #-}

-- | Get the name of the file used to store the xmonad window state.
stateFileName :: X FilePath
stateFileName = (</> "xmonad.state") <$> getXMonadDataDir

-- | 'recompile force', recompile the xmonad configuration file when
-- any of the following apply:
--
--      * force is 'True'
--
--      * the xmonad executable does not exist
--
--      * the xmonad executable is older than xmonad.hs or any file in
--        the @lib@ directory (under the configuration directory).
--
-- The -i flag is used to restrict recompilation to the xmonad.hs file only,
-- and any files in the aforementioned @lib@ directory.
--
-- Compilation errors (if any) are logged to the @xmonad.errors@ file
-- in the xmonad data directory.  If GHC indicates failure with a
-- non-zero exit code, an xmessage displaying that file is spawned.
--
-- 'False' is returned if there are compilation errors.
--
recompile :: MonadIO m => Dirs -> Bool -> m Bool
recompile Dirs{ cfgDir, dataDir } force = io $ do
    let binn = "xmonad-"++arch++"-"++os
        bin  = dataDir </> binn
        err  = dataDir </> "xmonad.errors"
        src  = cfgDir  </> "xmonad.hs"
        lib  = cfgDir  </> "lib"
        buildscript = cfgDir </> "build"

    libTs <- traverse getModTime . Prelude.filter isSource =<< allFiles lib
    srcT <- getModTime src
    binT <- getModTime bin

    useBuildscript <- do
      exists <- doesFileExist buildscript
      if exists
        then do
          isExe <- isExecutable buildscript
          if isExe
            then do
              trace $ "XMonad will use build script at " <> show buildscript <> " to recompile."
              pure True
            else do
              trace $ unlines
                [ "XMonad will not use build script, because " <> show buildscript <> " is not executable."
                , "Suggested resolution to use it: chmod u+x " <> show buildscript
                ]
              pure False
        else do
          trace $
            "XMonad will use ghc to recompile, because " <> show buildscript <> " does not exist."
          pure False

    shouldRecompile <-
      if useBuildscript || force
        then pure True
        else if any (binT <) (srcT : libTs)
          then do
            trace "XMonad doing recompile because some files have changed."
            pure True
          else do
            trace "XMonad skipping recompile because it is not forced (e.g. via --recompile), and neither xmonad.hs nor any *.hs / *.lhs / *.hsc files in lib/ have been changed."
            pure False

    if shouldRecompile
      then do
        -- temporarily disable SIGCHLD ignoring:
        uninstallSignalHandlers
        status <- bracket (openFile err WriteMode) hClose $ \errHandle ->
            waitForProcess =<< if useBuildscript
                               then compileScript bin cfgDir buildscript errHandle
                               else compileGHC bin cfgDir errHandle

        -- re-enable SIGCHLD:
        installSignalHandlers

        -- now, if it fails, run xmessage to let the user know:
        if status == ExitSuccess
            then trace "XMonad recompilation process exited with success!"
            else do
                ghcErr <- readFile err
                let msg =
                      unlines $
                        ["Error detected while loading xmonad configuration file: " <> src]
                        <> lines (if null ghcErr then show status else ghcErr)
                        <> ["","Please check the file for errors."]
                -- nb, the ordering of printing, then forking, is crucial due to
                -- lazy evaluation
                hPutStrLn stderr msg
                forkProcess $ executeFile "xmessage" True ["-default", "okay", replaceUnicode msg] Nothing
                pure ()
        pure (status == ExitSuccess)
      else pure True
 where getModTime f = E.catch (Just <$> getModificationTime f) (\(SomeException _) -> pure Nothing)
       isSource = flip elem [".hs",".lhs",".hsc"] . takeExtension
       isExecutable f = E.catch (executable <$> getPermissions f) (\(SomeException _) -> pure False)
       allFiles t = do
            let prep = fmap (t</>) . Prelude.filter (`notElem` [".",".."])
            cs <- prep <$> E.catch (getDirectoryContents t) (\(SomeException _) -> pure [])
            ds <- filterM doesDirectoryExist cs
            fold . ((cs \\ ds) :) <$> traverse allFiles ds
       -- Replace some of the unicode symbols GHC uses in its output
       replaceUnicode = fmap $ \c -> case c of
           '\8226' -> '*'  -- •
           '\8216' -> '`'  -- ‘
           '\8217' -> '`'  -- ’
           _ -> c
       compileGHC bin dir errHandle =
         runProcess "ghc" ["--make"
                          , "xmonad.hs"
                          , "-i"
                          , "-ilib"
                          , "-fforce-recomp"
                          , "-main-is", "main"
                          , "-v0"
                          , "-o", bin
                          ] (Just dir) Nothing Nothing Nothing (Just errHandle)
       compileScript bin dir script errHandle =
         runProcess script [bin] (Just dir) Nothing Nothing Nothing (Just errHandle)

-- | Conditionally run an action, using a @Maybe a@ to decide.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = for_
{-# DEPRECATED whenJust "Use for_" #-}

-- | Conditionally run an action, using a 'X' event to decide
whenX :: Monad m => m Bool -> m () -> m ()
whenX mb ma = mb >>= flip when ma
-- whenX mb mx = do
    -- b <- mb
    -- if b then mx else pure mempty

-- | A 'trace' for the 'X' monad. Logs a string to stderr. The result may
-- be found in your .xsession-errors file
trace :: MonadIO m => String -> m ()
trace = io . hPutStrLn stderr

-- | Ignore SIGPIPE to avoid termination when a pipe is full, and SIGCHLD to
-- avoid zombie processes, and clean up any extant zombie processes.
installSignalHandlers :: MonadIO m => m ()
installSignalHandlers = io $ do
    installHandler openEndedPipe Ignore Nothing
    installHandler sigCHLD Ignore Nothing
    (try :: IO a -> IO (Either SomeException a))
      . fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    pure ()

uninstallSignalHandlers :: MonadIO m => m ()
uninstallSignalHandlers = io
    $  installHandler openEndedPipe Default Nothing
    *> installHandler sigCHLD Default Nothing
    $> ()
