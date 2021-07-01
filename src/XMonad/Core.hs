{-# LANGUAGE ExistentialQuantification
           , FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , DeriveDataTypeable
           , NamedFieldPuns
           , DeriveTraversable
           , ScopedTypeVariables
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

module XMonad.Core (
    X, WindowSet, WindowSpace, WorkspaceId,
    ScreenId(..), ScreenDetail(..), XState(..),
    XConf(..), XConfig(..), LayoutClass(..),
    Layout(..), readsLayout, Typeable, Message,
    SomeMessage(..), fromMessage, LayoutMessages(..),
    StateExtension(..), ExtensionClass(..), ConfExtension(..),
    runX, catchX, userCode, userCodeDef, io, catchIO, installSignalHandlers, uninstallSignalHandlers,
    withDisplay, withWindowSet, isRoot, runOnWorkspaces,
    getAtom,
    spawn, spawnPID, xfork, -- spawnPipe,
    recompile, trace, whenJust, whenX,
    stateFileName,
    atom_WM_STATE, atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW, atom_WM_TAKE_FOCUS, withWindowAttributes',
    ManageHook, Query(..), runQuery, Directories'(..), Directories, getDirectories,
    -- XConf Optics
    _config, _currentEvent, _directories, _display, _theRoot, _focusedBorder, _normalBorder, _buttonActions, _keyActions, _mouseFocused, _mousePosition,
    -- XConfig Optics
    _layoutHook,
    -- XState Optics
    _dragging, _extensibleState, _mapped, _waitingUnmap, _numberlockMask, _windowset,
    -- Deprecated
    withWindowAttributes, getXMonadDir, getXMonadCacheDir, getXMonadDataDir,
  ) where

import XMonad.StackSet hiding (modify)
import XMonad.Internal.Optics

import Prelude hiding (fail)
import Control.Exception (fromException, try, throw, finally, SomeException(..))
import qualified Control.Exception as E
import Control.Applicative (Alternative, (<|>), empty, liftA2)
import Control.Monad (filterM, guard, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Fail (MonadFail (fail))
import Control.Monad.State.Strict (MonadState (..), gets)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Writer (MonadWriter (..))
import Data.Semigroup
import Data.Functor ((<&>))
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Function (fix)
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
-- import qualified System.Process.Typed as Process
import System.Directory
import System.Exit -- ExitCode
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getWindowAttributes, WindowAttributes, Event)
import Data.Typeable
import Data.List ((\\))
import Data.Maybe (isJust)

import qualified Data.Map.Strict as M
import qualified Data.Set as S


-- | XState, the (mutable) window manager state.
data XState = XState
    { windowset        :: !WindowSet                     -- ^ workspace list
    , mapped           :: !(S.Set Window)                -- ^ the Set of mapped windows
    , waitingUnmap     :: !(M.Map Window Int)            -- ^ the number of expected UnmapEvents
    , dragging         :: !(Maybe (Position -> Position -> X (), X ()))
    , numberlockMask   :: !KeyMask                       -- ^ The numlock modifier
    , extensibleState  :: !(M.Map String (Either String StateExtension))
    -- ^ stores custom state information.
    --
    -- The module "XMonad.Util.ExtensibleState" in xmonad-contrib
    -- provides additional information and a simple interface for using this.
    }

-- XState Optics
-- XState Lenses
_windowset :: (Functor m)=> (WindowSet -> m WindowSet) -> XState -> m XState
_windowset f xst =
    f (windowset xst) <&> \ windowset' -> xst{ windowset = windowset' }

_mapped ::
    (Functor m)=> (S.Set Window -> m (S.Set Window)) -> XState -> m XState
_mapped f xst =
    f (mapped xst) <&> \ mapped' -> xst{ mapped = mapped' }

_waitingUnmap ::
    (Functor m)=>
    (M.Map Window Int -> m (M.Map Window Int)) -> XState -> m XState
_waitingUnmap f xst =
    f (waitingUnmap xst) <&> \ waiting' -> xst{ waitingUnmap = waiting' }

_dragging ::
    (Functor m)=>
    (Maybe (Position -> Position -> X (), X ()) -> m (Maybe (Position -> Position -> X (), X ()))) ->
    XState -> m XState
_dragging f xst =
    f (dragging xst) <&> \ dragging' -> xst{ dragging = dragging' }

_numberlockMask :: (Functor m)=> (KeyMask -> m KeyMask) -> XState -> m XState
_numberlockMask f xst =
    f (numberlockMask xst) <&> \ numlock' -> xst{ numberlockMask = numlock' }

_extensibleState ::
    (Functor m)=>
    (M.Map String (Either String StateExtension) -> m (M.Map String (Either String StateExtension))) ->
    XState -> m XState
_extensibleState f xst =
    f (extensibleState xst) <&> \ state' -> xst{ extensibleState = state' }

-- | XConf, the (read-only) window manager configuration.
data XConf = XConf
    { display       :: Display        -- ^ the X11 display
    , config        :: !(XConfig Layout)       -- ^ initial user configuration
    , theRoot       :: !Window        -- ^ the root window
    , normalBorder  :: !Pixel         -- ^ border color of unfocused windows
    , focusedBorder :: !Pixel         -- ^ border color of the focused window
    , keyActions    :: !(M.Map (KeyMask, KeySym) (X ()))
                                      -- ^ a mapping of key presses to actions
    , buttonActions :: !(M.Map (KeyMask, Button) (Window -> X ()))
                                      -- ^ a mapping of button presses to actions
    , mouseFocused :: !Bool           -- ^ was refocus caused by mouse action?
    , mousePosition :: !(Maybe (Position, Position))
                                      -- ^ position of the mouse according to
                                      -- the event currently being processed
    , currentEvent :: !(Maybe Event)  -- ^ event currently being processed
    , directories  :: !Directories    -- ^ directories to use
    }

_display ::
    (Functor m)=>
    (Display -> m Display) ->
    XConf -> m XConf
_display f s =
    f (display s) <&> \ display' -> s{ display = display' }

_config ::
    (Functor m)=>
    (XConfig Layout -> m (XConfig Layout)) ->
    XConf -> m XConf
_config f s = f (config s) <&> \ config' -> s{ config = config' }

_theRoot ::
    (Functor m)=>
    (Window -> m Window) ->
    XConf -> m XConf
_theRoot f s =
    f (theRoot s) <&> \ theRoot' -> s{ theRoot = theRoot' }

_normalBorder ::
    (Functor m)=>
    (Pixel -> m Pixel) ->
    XConf -> m XConf
_normalBorder f s =
    f (normalBorder s) <&>
    \ normalBorder' -> s{ normalBorder = normalBorder' }

_focusedBorder ::
    (Functor m)=>
    (Pixel -> m Pixel) ->
    XConf -> m XConf
_focusedBorder f s =
    f (focusedBorder s) <&>
    \ focusedBorder' -> s{ focusedBorder = focusedBorder' }

_keyActions ::
    (Functor m)=>
    (M.Map (KeyMask, KeySym) (X ()) -> m (M.Map (KeyMask, KeySym) (X ()))) ->
    XConf -> m XConf
_keyActions f s =
    f (keyActions s) <&> \ keyActions' -> s{ keyActions = keyActions' }

_buttonActions ::
    (Functor m)=>
    (M.Map (KeyMask, Button) (Window -> X ()) -> m (M.Map (KeyMask, Button) (Window -> X ()))) ->
    XConf -> m XConf
_buttonActions f s =
    f (buttonActions s) <&>
    \ buttonActions' -> s{ buttonActions = buttonActions' }

_currentEvent ::
    (Functor m)=>
    (Maybe Event -> m (Maybe Event)) ->
    XConf -> m XConf
_currentEvent f s =
    f (currentEvent s) <&> \ currentEvent' -> s{ currentEvent = currentEvent' }

_mouseFocused ::
    (Functor m)=>
    (Bool -> m Bool) ->
    XConf -> m XConf
_mouseFocused f s =
    f (mouseFocused s) <&>
    \ mouseFocused' -> s{ mouseFocused = mouseFocused' }

_mousePosition ::
    (Functor m)=>
    (Maybe (Position, Position) -> m (Maybe (Position, Position))) ->
    XConf -> m XConf
_mousePosition f s =
    f (mousePosition s) <&>
    \ mousePosition' -> s{ mousePosition = mousePosition' }

_directories ::
    (Functor m)=>
    (Directories -> m Directories) ->
    XConf -> m XConf
_directories f s =
    f (directories s) <&> \ directories' -> s{ directories = directories' }

-- todo, better name
data XConfig l = XConfig
    { normalBorderColor  :: !String              -- ^ Non focused windows border color. Default: \"#dddddd\"
    , focusedBorderColor :: !String              -- ^ Focused windows border color. Default: \"#ff0000\"
    , terminal           :: !String              -- ^ The preferred terminal application. Default: \"xterm\"
    , layoutHook         :: !(l Window)     -- ^ The available layouts
    , manageHook         :: !ManageHook          -- ^ The action to run when a new window is opened
    , handleEventHook    :: !(Event -> X All)    -- ^ Handle an X event, returns (All True) if the default handler
                                                 -- should also be run afterwards. mappend should be used for combining
                                                 -- event hooks in most cases.
    , workspaces         :: ![String]            -- ^ The list of workspaces' names
    , modMask            :: !KeyMask             -- ^ the mod modifier
    , keys               :: !(XConfig Layout -> M.Map (ButtonMask,KeySym) (X ()))
                                                 -- ^ The key binding: a map from key presses and actions
    , mouseBindings      :: !(XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ()))
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
    , extensibleConf     :: !(M.Map TypeRep ConfExtension)
                                                 -- ^ Stores custom config information.
                                                 --
                                                 -- The module "XMonad.Util.ExtensibleConf" in xmonad-contrib
                                                 -- provides additional information and a simple interface for using this.
    }

_layoutHook ::
    (Functor m)=>
    (l Window -> m (l' Window)) ->
    XConfig l -> m (XConfig l')
_layoutHook f xcfg =
    f (layoutHook xcfg) <&> \ layoutHook' -> xcfg{ layoutHook = layoutHook' }

_extensibleConf ::
    (Functor m)=>
    (M.Map TypeRep ConfExtension -> m (M.Map TypeRep ConfExtension)) ->
    XConfig l -> m (XConfig l)
_extensibleConf f xcfg =
    f (extensibleConf xcfg) <&>
    \ extensibleConf' -> xcfg{ extensibleConf = extensibleConf' }

type WindowSet   = StackSet  WorkspaceId (Layout Window) Window ScreenId ScreenDetail
type WindowSpace = Workspace WorkspaceId (Layout Window) Window

-- | Virtual workspace indices
type WorkspaceId = String

-- | Physical screen indices
newtype ScreenId    = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | The 'Rectangle' with screen dimensions
newtype ScreenDetail = SD { screenRect :: Rectangle }
    deriving (Eq, Show, Read)

_screenRect ::
   (Functor m)=>
   (Rectangle -> m Rectangle) -> ScreenDetail -> m ScreenDetail
_screenRect f = fmap SD . f . screenRect

------------------------------------------------------------------------

-- | The X monad, 'WriterT', 'ReaderT' and 'StateT' transformers over 'IO'
-- encapsulating the window manager configuration and state,
-- respectively.
--
-- Dynamic components may be retrieved with 'get', static components
-- with 'ask'. With newtype deriving we get readers and state monads
-- instantiated on 'XConf' and 'XState' automatically.
--
-- To say that you need a refresh, use @tell (Any True)@.
-- To determine whether you need a refresh, use @getAny . snd <$> listen@.
newtype X a = X (XConf -> XState -> Any -> IO (XResult a))
  deriving (Functor)

unX :: X a -> XConf -> XState -> Any -> IO (XResult a)
unX (X act) = act

-- | Run the 'X' monad, given a chunk of 'X' monad code, and an initial state
-- Return the result, and final state
runX :: XConf -> XState -> X a -> IO (a, XState)
runX conf stat (X act)= do
    XResult s _ a <- act conf stat mempty
    pure (a, s)

data XResult a = XResult !XState !Any a
  deriving (Functor, Typeable)

instance Monad X where
    X act >>= f = X $ \ r s w -> do
        XResult s' w' a <- act r s w
        unX (f a) r s' w'
    {-# INLINE (>>=) #-}

instance Applicative X where
    pure a = X $ \ _ s w -> pure (XResult s w a)
    {-# INLINE pure #-}
    liftA2 f (X act1) (X act2) = X $ \ r s w -> do
        XResult s' w' a <- act1 r s w
        XResult s'' w'' b <- act2 r s' w'
        pure (XResult s'' w'' (f a b))
    {-# INLINE liftA2 #-}

instance Alternative X where
    empty = X $ \ _ _ _ -> empty
    {-# INLINE empty #-}
    X act1 <|> X act2 = X $ \ r s w -> act1 r s w <|> act2 r s w
    {-# INLINE (<|>) #-}

instance MonadFail X where
    fail message = X $ \ _ _ _ -> fail message
    {-# INLINE fail #-}

instance MonadIO X where
    liftIO act = X $ \ _ s w -> XResult s w <$> act
    {-# INLINE liftIO #-}

instance MonadReader XConf X where
    reader f = X $ \ r s w -> pure (XResult s w (f r))
    {-# INLINE reader #-}
    ask = X $ \ r s w -> pure (XResult s w r)
    {-# INLINE ask #-}
    local f (X act) = X $ \ r s w -> act (f r) s w
    {-# INLINE local #-}

instance MonadState XState X where
    state f = X $ \ _ s w -> case f s of (a, s') -> pure (XResult s' w a)
    {-# INLINE state #-}
    get = X $ \ _ s w -> pure (XResult s w s)
    {-# INLINE get #-}
    put s = X $ \ _ _ w -> pure (XResult s w ())
    {-# INLINE put #-}

instance MonadWriter Any X where
    writer (a, w) = X $ \ _ s w' -> pure (XResult s (w <> w') a)
    {-# INLINE writer #-}
    tell w = X $ \ _ s w' -> pure (XResult s (w <> w') ())
    {-# INLINE tell #-}
    listen (X act) = X $ \ r s w -> do
        XResult s' w' a <- act r s w
        pure (XResult s' w' (a, w'))
    {-# INLINE listen #-}
    pass (X act) = X $ \ r s w -> do
        XResult s' w' (a, f) <- act r s w
        pure (XResult s' (f w') a)
    {-# INLINE pass #-}

instance Semigroup a => Semigroup (X a) where
    (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (X a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance Default a => Default (X a) where
    def = pure def

type ManageHook = Query (Endo WindowSet)
newtype Query a = Query (ReaderT Window X a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Window)

runQuery :: Query a -> Window -> X a
runQuery (Query m) w = runReaderT m w

_runQuery ::
    (Functor m)=>
    ((Window -> X a) -> m (Window -> X a)) -> Query a -> m (Query a)
_runQuery f = fmap (Query . ReaderT) . f . runQuery

instance Semigroup a => Semigroup (Query a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Query a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance Default a => Default (Query a) where
    def = pure def


-- | Run an action in the 'X' monad; in case of exception, catch it, log it
-- to stderr, and run the error case.
catchX :: X a -> X a -> X a
catchX job errcase = do
    result <- tryX job
    case result of
      Left e ->
          case fromException e of
            Just (_ :: ExitCode) -> throw e
            Nothing -> liftIO (hPrint stderr e *> hFlush stderr) *> errcase
      Right x -> pure x

-- | Run an action in 'X'; if it produces an error, return the error wrapped in 'Left' and leave the 'XState' unchanged; otherwise wrap the result in 'Right' and use the new 'XState'.
tryX :: (E.Exception e)=> X a -> X (Either e a)
tryX act = do
    conf <- ask
    stat <- get
    result <- liftIO $ E.try $ unX act conf stat mempty
    case result of
      Left e -> pure $ Left e
      Right (XResult stat' refr' x) -> Right x <$ (put stat' *> tell refr')

-- -- | Run an action in 'X'; if it produces an error, run a cleanup action, then throw the error; otherwise return the result of the action.
-- onExceptionX :: X a -> X () -> X a
-- act `onExceptionX` cleanup = do
--     result <- tryX act
--     case result of
--         Left (e :: SomeException) -> do
--             (_ :: Either SomeException ()) <- tryX cleanup
--             io $ E.throwIO e
--         Right x -> pure x

-- -- | Run an action in 'X', then always run a cleanup action and return the result of the first action.
-- finallyX :: X a -> X () -> X a
-- act `finallyX` cleanup = do
--     x <- onExceptionX act cleanup
--     cleanup
--     pure x

-- bracketX :: X a -> (a -> X ()) -> (a -> X b) -> X b
-- bracketX setup cleanup act = do
--     conf <- ask
--     resource <- setup
--     stat <- get
--     (result', stat''') <- io $ E.mask $ \ unmask -> do
--         result <- unmask $ E.try $ runX conf stat (act resource)
--         case result of
--             Left e -> do
--                 done <- E.try $ runX conf stat $ cleanup resource
--                 unmask $ case done of
--                     Left (_ :: E.SomeException) -> pure (Left e, stat)
--                     Right ((), stat') -> pure (Left e, stat')
--             Right (x, stat') -> do
--                 done <- E.try $ runX conf stat' $ cleanup resource
--                 unmask $ case done of
--                     Left (e :: E.SomeException) -> pure (Left e, stat')
--                     Right ((), stat'') -> pure (Right x, stat'')
--     put stat'''
--     case result' of
--         Left e -> io $ E.throwIO e
--         Right x -> pure x

-- | Execute the argument, catching all exceptions.  Either this function or
-- 'catchX' should be used at all callsites of user customized code.
userCode :: X a -> X (Maybe a)
userCode act = userCodeDef Nothing (Just <$> act)

-- | Same as userCode but with a default argument to return instead of using
-- Maybe, provided for convenience.
userCodeDef :: a -> X a -> X a
userCodeDef defValue act = act `catchX` pure defValue

-- ---------------------------------------------------------------------
-- Convenient wrappers to state

-- | Run a monad action with the current display settings
-- Provided for backward compatibility; current style is to directly use @asks display@ instead.
withDisplay :: (Display -> X a) -> X a
withDisplay   f = asks display >>= f

-- | Run a monadic action with the current stack set
-- Provided for backward compatibility; current style is to directly use @gets windowset@ instead.
withWindowSet :: (WindowSet -> X a) -> X a
withWindowSet f = gets windowset >>= f

-- | Safely access window attributes.
withWindowAttributes :: Display -> Window -> (WindowAttributes -> X ()) -> X ()
withWindowAttributes disp window f =
    userCodeDef () $ liftIO (getWindowAttributes disp window) >>= f
{-# DEPRECATED withWindowAttributes "Use withWindowAttributes'" #-}

withWindowAttributes' :: Window -> (WindowAttributes -> X ()) -> X ()
withWindowAttributes' window f = do
    disp <- asks display
    withWindowAttributes disp window f

-- | True if the given window is the root window
isRoot :: Window -> X Bool
isRoot w = asks $ (w ==) . theRoot

-- | Wrapper for the common case of atom internment
getAtom :: String -> X Atom
getAtom str = asks display >>= \ dpy -> io $ internAtom dpy str False

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
-- All of the methods have default implementations, so there is no
-- minimal complete definition.  They do, however, have a dependency
-- structure by default; this is something to be aware of should you
-- choose to implement one of these methods.  Here is how a minimal
-- complete definition would look like if we did not provide any default
-- implementations:
--
-- * 'runLayout' || (('doLayout' || 'pureLayout') && 'emptyLayout')
--
-- * 'handleMessage' || 'pureMessage'
--
-- * 'description'
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
    handleMessage l  = pure . pureMessage l

    -- | Respond to a message by (possibly) changing our layout, but
    -- taking no other action.  If the layout changes, the screen will
    -- be refreshed.
    pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
    pureMessage _ _  = Nothing

    -- | This should be a human-readable string that is used when
    -- selecting layouts by name.  The default implementation is
    -- 'show', which is in some cases a poor default.
    description :: layout a -> String
    description      = show

--    toLayout :: layout a -> Layout a
--    toLayout = Layout

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
class Typeable a => Message a where
   -- toMessage :: a -> SomeMessage
   -- toMessage = SomeMessage
   -- fromMessage :: SomeMessage -> Maybe a
   -- fromMessage (SomeMessage x) = cast x

-- |
-- A wrapped value of some type in the 'Message' class.
--
data SomeMessage = forall a. Message a => SomeMessage a deriving Typeable

-- instance Message SomeMessage where
--    toMessage = id
--    fromMessage = Just

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
    deriving Eq

instance Message LayoutMessages

-- ---------------------------------------------------------------------
-- Extensible state/config
--

-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a => ExtensionClass a where
    {-# MINIMAL initialValue #-}
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
data StateExtension =
    forall a. ExtensionClass a => StateExtension a
    -- ^ Non-persistent state extension
  | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a
    -- ^ Persistent extension

-- | Existential type to store a config extension.
data ConfExtension = forall a. Typeable a => ConfExtension a

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
xfork x = io . forkProcess . finally nullStdin $ do
                uninstallSignalHandlers
                createSession
                x
 where
    nullStdin = do
        fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
        dupTo fd stdInput
        closeFd fd

-- spawnPipe :: MonadIO m => [Char] -> m Handle
-- spawnPipe command = Process.getStdin <$> Process.startProcess cfg
--    where
--    cfg = Process.proc "/bin/sh" ["-c", command]
--          & Process.setStdin Process.createPipe

-- | This is basically a map function, running a function in the 'X' monad on
-- each workspace with the output of that function being the modified workspace.
runOnWorkspaces ::
   (MonadState XState m)=> (WindowSpace -> m WindowSpace) -> m ()
runOnWorkspaces job = _windowset .=<< _workspaces %%~ job =<< use _windowset
  where
    (.=<<) = (<~)
    infixr 1 .=<<
   -- WARNING: This has changed the order of runOnWorkspaces. Carefully check whether anything depended on acting on hidden workspaces first!
{-# SPECIALIZE runOnWorkspaces :: (WindowSpace -> X WindowSpace) -> X () #-}

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
-- For how these directories are chosen, see 'getDirectories'.
--
data Directories' a = Directories
    { dataDir  :: !a
    , cfgDir   :: !a
    , cacheDir :: !a
    }
    deriving (Show, Functor, Foldable, Traversable)

-- | Convenient type alias for the most common case in which one might
-- want to use the 'Directories' type.
type Directories = Directories' FilePath

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
getDirectories :: IO Directories
getDirectories = xmEnvDirs <|> xmDirs <|> xdgDirs
  where
    -- | Check for xmonad's environment variables first
    xmEnvDirs :: IO Directories
    xmEnvDirs = do
        let xmEnvs = Directories{ dataDir  = "XMONAD_DATA_DIR"
                         , cfgDir   = "XMONAD_CONFIG_DIR"
                         , cacheDir = "XMONAD_CACHE_DIR"
                         }
        maybe empty pure . sequenceA =<< traverse getEnv xmEnvs
        -- Note: This will ignore all environment variables if at least one is not set.

    -- | Check whether the config file or a build script is in the
    -- @~\/.xmonad@ directory
    xmDirs :: IO Directories
    xmDirs = do
        xmDir <- getAppUserDataDirectory "xmonad"
        conf  <- doesFileExist $ xmDir </> "xmonad.hs"
        build <- doesFileExist $ xmDir </> "build"

        -- Place *everything* in ~/.xmonad if yes
        guard $ conf || build
        pure Directories{ dataDir = xmDir, cfgDir = xmDir, cacheDir = xmDir }

    -- | Use XDG directories as a fallback
    xdgDirs :: IO Directories
    xdgDirs =
        for Directories{ dataDir = XdgData, cfgDir = XdgConfig, cacheDir = XdgCache }
            $ \dir -> do d <- getXdgDirectory dir "xmonad"
                         d <$ createDirectoryIfMissing True d

-- | Return the path to the xmonad configuration directory.
getXMonadDir :: X String
getXMonadDir = asks (cfgDir . directories)
{-# DEPRECATED getXMonadDir "Use `asks (cfgDir . directories)' instead." #-}

-- | Return the path to the xmonad cache directory.
getXMonadCacheDir :: X String
getXMonadCacheDir = asks (cacheDir . directories)
{-# DEPRECATED getXMonadCacheDir "Use `asks (cacheDir . directories)' instead." #-}

-- | Return the path to the xmonad data directory.
getXMonadDataDir :: X String
getXMonadDataDir = asks (dataDir . directories)
{-# DEPRECATED getXMonadDataDir "Use `asks (dataDir . directories)' instead." #-}

-- | Get the name of the file used to store the xmonad window state.
stateFileName :: X FilePath
stateFileName = asks $ (</> "xmonad.state") . dataDir . directories

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
recompile :: MonadIO m => Directories -> Bool -> m Bool
recompile Directories{ cfgDir, dataDir } force = io $ do
    let binn = "xmonad-" <> arch <> "-" <> os
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
        status <- withFile err WriteMode $ \errHandle ->
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
                let msg = unlines $
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
  where
    getModTime f = E.catch (Just <$> getModificationTime f) (\(SomeException _) -> pure Nothing)
    isSource = flip elem [".hs",".lhs",".hsc"] . takeExtension
    isExecutable f = E.catch (executable <$> getPermissions f) (\(SomeException _) -> pure False)
    allFiles t = do
        let prep = fmap (t </>) . Prelude.filter (`notElem` [".",".."])
        cs <- prep <$> E.catch (getDirectoryContents t) (\(SomeException _) -> pure [])
        ds <- filterM doesDirectoryExist cs
        concat . ((cs \\ ds):) <$> traverse allFiles ds
    -- Replace some of the unicode symbols GHC uses in its output
    replaceUnicode = fmap $ \c -> case c of
        '\8226' -> '*'  -- •
        '\8216' -> '`'  -- ‘
        '\8217' -> '`'  -- ’
        _ -> c
    compileGHC bin dir errHandle = runProcess "ghc"
        ["--make"
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

-- | Conditionally run an action, using a monadic event to decide
whenX :: Monad m => m Bool -> m () -> m ()
whenX a f = a >>= \b -> when b f
{-# INLINE whenX #-}

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
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    pure ()

uninstallSignalHandlers :: MonadIO m => m ()
uninstallSignalHandlers = io $ do
    installHandler openEndedPipe Default Nothing
    installHandler sigCHLD Default Nothing
    pure ()
