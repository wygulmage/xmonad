{-# LANGUAGE MultiWayIf
           , PatternGuards
           , ScopedTypeVariables #-}


module XMonad.ExtensibleState.Ewmh  (
    FullscreenBroadcast (..),
    FullscreenChanged (..),
    ewmh,
    ewmhDesktopStartup,
    ewmhDesktopsLogHook,
    ewmhDesktopsEventHook,
    fullscreenEventHook,
    isFullscreen,
    ) where

import qualified Codec.Binary.UTF8.String as UTF8

import Control.Monad (when)
import Control.Monad.Reader (ask, asks)
import qualified Control.Monad.State as State

import Data.Foldable (fold, foldl', for_, traverse_)
import Data.Int (Int32)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Semigroup (All (All))
import Data.Typeable (Typeable)
import qualified Data.Set as Set

import XMonad
    ( ExtensionClass (initialValue)
    , Query
    , Message, broadcastMessage, sendMessage, sendMessageWithNoRefresh
    , Layout
    , Atom, Event (..), WorkspaceId, Window, WindowSpace, X, XConfig
    , windows
    , display, theRoot, handleEventHook, logHook, startupHook, windowset
    , killWindow, trace
    , none
    , aTOM, cARDINAL, wINDOW
    , liftX
    )
import qualified XMonad.StackSet as W
import qualified XMonad.ExtensibleState as ES
import XMonad.X11

data FullscreenBroadcast
    = AddFullscreen !Window
    | RemoveFullscreen !Window
  deriving (Eq, Show, Typeable)
-- ^ This message is sent to all layouts when a window becomes or ceases to be fullscreen.

instance Message FullscreenBroadcast

data FullscreenChanged = FullscreenChanged -- ^ This message is sent to the current layout when a window becomes or ceases to be fullscreen.
  deriving (Eq, Show, Typeable)

instance Message FullscreenChanged

data EwmhCache = EwmhCache
    { desktopNames   :: ![String]     -- ^ Workspace IDs, lexicographically sorted
    , currentDesktop :: !Int32        -- ^ index of the focused desktop in desktopNames
    , clientList     :: ![Window]     -- ^ Windows managed by xmonad, ordered by ???
    , windowDesktops :: ![[Window]]   -- ^ tiled Windows grouped by Workspace, ordered by ???
    , activeWindow   :: !Window       -- ^ focused Window
    , fullscreenCache :: Set.Set Window -- ^ fullscreen windows
    }

instance ExtensionClass EwmhCache where
    initialValue = EwmhCache
        { desktopNames = []
        , currentDesktop = 0
        , clientList = []
        , windowDesktops = []
        , activeWindow = none
        , fullscreenCache = Set.empty
        }

ewmh :: XConfig a -> XConfig a
ewmh cfg = cfg
    { startupHook = ewmhDesktopStartup <> startupHook cfg
    , handleEventHook = fullscreenEventHook <> ewmhDesktopsEventHook <> handleEventHook cfg
    , logHook = ewmhDesktopsLogHook <> logHook cfg
    }

ewmhDesktopStartup :: X ()
ewmhDesktopStartup = addSupported $
    "_NET_WM_STATE_HIDDEN" :
    "_NET_NUMBER_OF_DESKTOPS" :
    "_NET_CLIENT_LIST" :
    "_NET_CLIENT_LIST_STACKING" :
    "_NET_CURRENT_DESKTOP" :
    "_NET_DESKTOP_NAMES" :
    "_NET_ACTIVE_WINDOW" :
    "_NET_WM_DESKTOP" :
    "_NET_WM_STRUT" :
    "_NET_WM_STATE" :
    "_NET_WM_STATE_FULLSCREEN" :
    []

ewmhDesktopsLogHook :: X ()
{- ^ Update EWMH state.
-}
ewmhDesktopsLogHook = do
    es <- ES.get
    s <- State.gets windowset
    let
      ws :: [W.Workspace WorkspaceId (Layout Window) Window]
      ws = List.sortOn W.tag $ W.workspaces s

    let
      desktop_names :: [WorkspaceId]
      desktop_names = fmap W.tag ws
    when (desktopNames es /= desktop_names) $ do
       setDesktopNames desktop_names
       let
         number_of_desktops :: Int32
         number_of_desktops = intToInt32 $ length $ desktopNames es
         number_of_desktops' :: Int32
         number_of_desktops' = intToInt32 $ length desktop_names
       when (number_of_desktops /= number_of_desktops') $
           setNumberOfDesktops number_of_desktops'

    let
      client_list :: [Window]
      client_list = foldl' (\ cs c -> List.union cs $ W.integrate' $ W.stack c) [] ws
    when (clientList es /= client_list) $ setClientList client_list

    let
      currentDesktop' :: Int32
      currentDesktop' = maybe (currentDesktop es) intToInt32 $
          (W.tag . W.workspace . W.current) s `List.elemIndex` desktop_names
    when (currentDesktop es /= currentDesktop') $
        setCurrentDesktop currentDesktop'

    let
      window_desktops :: [[Window]]
      window_desktops = W.integrate' . W.stack <$> ws
      itraverse_ f = loop 0
        where
        loop i xs = case xs of
            x : xs' -> f i x *> loop (i + 1) xs'
            []      -> pure ()
    when (windowDesktops es /= window_desktops) $
        itraverse_ (traverse_ . setWindowDesktop) window_desktops

    let
      activeWindow' :: Window
      activeWindow' = fromMaybe none $ W.peek s
    when (activeWindow es /= activeWindow') $ setActiveWindow activeWindow'

    ES.put es
        { windowDesktops = window_desktops
        , desktopNames = desktop_names
        , clientList = client_list
        , currentDesktop = currentDesktop'
        , activeWindow = activeWindow'
        }


ewmhDesktopsEventHook :: Event -> X All
ewmhDesktopsEventHook e = All True <$ handle id e

handle :: ([WindowSpace] -> [WindowSpace]) -> Event -> X ()
handle f msg@ClientMessageEvent{} = do
    ws <- State.gets $ f . W.workspaces . windowset

    _NET_CURRENT_DESKTOP <- getAtom "_NET_CURRENT_DESKTOP"
    _NET_WM_DESKTOP <- getAtom "_NET_WM_DESKTOP"
    _NET_ACTIVE_WINDOW <- getAtom "_NET_ACTIVE_WINDOW"
    _NET_CLOSE_WINDOW <- getAtom "_NET_CLOSE_WINDOW"

    case ev_message_type msg of
        mt
            | mt == _NET_CURRENT_DESKTOP
            -> case ev_data msg of
                    n : _ | n' <- cIntToInt n
                          , 0 <= n'  &&  n' < length ws ->
                        windows . W.view . W.tag $ ws !! n'
                    _ ->
                        trace "Bad _NET_CURRENT_DESKTOP data"
            | mt == _NET_WM_DESKTOP
            -> case ev_data msg of
                    n : _ | n' <- cIntToInt n
                          , 0 <= n' && n' < length ws ->
                        windows . W.shiftWin (W.tag $ ws !! n') $ ev_window msg
                    _ ->
                        trace "Bad _NET_CURRENT_DESKTOP data"
            | mt == _NET_ACTIVE_WINDOW
            -> do -- FIXME: User config should decide how to honor client window activation requests.
                  windows . W.focusWindow $ ev_window msg
                  setActiveWindow $ ev_window msg
            | mt == _NET_CLOSE_WINDOW
            -> killWindow $ ev_window msg
            | otherwise
            -> pure ()
handle _ _ = pure ()

fullscreenEventHook :: Event -> X All
fullscreenEventHook ev@ClientMessageEvent
    { ev_window = win
    , ev_data = (act : dats)
    } = do
    _NET_WM_STATE <- getAtom "_NET_WM_STATE"
    let _NET_WM_STATE_32 = atomToWord32 _NET_WM_STATE
    _NET_WM_STATE_FULLSCREEN <- getAtom "_NET_WM_FULLSCREEN"
    let _NET_WM_STATE_FULLSCREEN_32 = atomToWord32 _NET_WM_STATE_FULLSCREEN
    let _NET_WM_STATE_FULLSCREEN_CInt = atomToCInt _NET_WM_STATE_FULLSCREEN

    dpy <- asks display
    (All True <$) . when (
        ev_event_display ev == dpy  &&
        ev_event_type ev == _NET_WM_STATE_32  &&
        _NET_WM_STATE_FULLSCREEN_CInt `elem` dats) $ do

        wstate <- fold <$> getWindowProperty32 _NET_WM_STATE win
        let
          isFull = _NET_WM_STATE_FULLSCREEN_32 `elem` wstate
          changeWindowState f = replaceWindowProperty aTOM _NET_WM_STATE (f wstate) win
        if
            | act == add || (act == toggle  &&  not isFull)
            -> do
                changeWindowState (_NET_WM_STATE_FULLSCREEN_32 :)
                -- windows $ W.float win $ W.RationalRect 0 0 1 1
                broadcastMessage $ AddFullscreen win
                sendMessage FullscreenChanged
            | act == remove || (act == toggle && not isFull)
            -> do
                changeWindowState $ List.delete _NET_WM_STATE_FULLSCREEN_32
                -- windows $ W.sink win
                broadcastMessage $ RemoveFullscreen win
                sendMessage FullscreenChanged
            | otherwise
            -> pure ()
  where
    remove = 0
    add = 1
    toggle = 2
fullscreenEventHook DestroyWindowEvent{ ev_window = w } = do
    -- When a window is destroyed, the layouts should remove that window
    -- from their states. Nothing to do with EWMH, but belongs with the other fullscreen events.
    broadcastMessage $ RemoveFullscreen w
    cw <- State.gets $ W.workspace . W.current . windowset
    sendMessageWithNoRefresh FullscreenChanged cw
    pure $ All True
fullscreenEventHook _ = pure $ All True

setActiveWindow :: Window -> X ()
setActiveWindow w = do
    r <- asks theRoot
    _NET_ACTIVE_WINDOW <- getAtom "_NET_ACTIVE_WINDOW"
    replaceWindowProperty wINDOW _NET_ACTIVE_WINDOW [windowToWord32 w] r
    pure () -- Ignore errors.

setNumberOfDesktops :: Int32 -> X ()
setNumberOfDesktops n = do
    r <- asks theRoot
    a <- getAtom "_NET_NUMBER_OF_DESKTOPS"
    replaceWindowProperty cARDINAL a [n] r
    pure () -- Ignore errors.

setCurrentDesktop :: Int32 -> X ()
setCurrentDesktop i = do
    r <- asks theRoot
    a <- getAtom "_NET_CURRENT_DESKTOP"
    replaceWindowProperty cARDINAL a [i] r
    pure () -- Ignore errors.

setDesktopNames :: (Foldable m)=> m String -> X ()
setDesktopNames names = do
   r <- asks theRoot
   a <- getAtom "_NET_DESKTOP_NAMES"
   c <- getAtom "UTF8_STRING"
   let names' = foldMap ((<> [0]) . UTF8.encode) names
   replaceWindowProperty c a names' r
   pure () -- Ignore errors.

setClientList :: [Window] -> X ()
setClientList clients = do
    r <- asks theRoot
    a <- getAtom "_NET_CLIENT_LIST"
    let clients' = fmap windowToWord32 clients
    replaceWindowProperty wINDOW a clients' r
    a' <- getAtom "_NET_CLIENT_LIST_STACKING"
    replaceWindowProperty wINDOW a' clients' r
    pure () -- Ignore errors.


setWindowDesktop :: Int32 -> Window -> X ()
setWindowDesktop i win = do
    _NET_WM_DESKTOP <- getAtom "_NET_WM_DESKTOP"
    replaceWindowProperty cARDINAL _NET_WM_DESKTOP [i] win
    pure () -- ignore errors

addSupported :: [String] -> X ()
addSupported properties = do
    newSupported <- traverse (fmap fromIntegral . getAtom) properties
    modifySupported $ List.union newSupported

modifySupported :: ([Atom] -> [Atom]) -> X ()
modifySupported f = do
    root <- asks theRoot
    _NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    oldSupported <- getWindowProperty _NET_SUPPORTED root
    let oldAtoms = word32ToAtom <$> fold oldSupported
        newSupported = atomToWord32 <$> f oldAtoms
    replaceWindowProperty aTOM _NET_SUPPORTED newSupported root
    pure () -- Ignore errors.

setSupported :: [String] -> X ()
setSupported supported = do
    root <- asks theRoot
    _NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    supported' <- traverse (fmap atomToWord32 . getAtom) supported
    replaceWindowProperty aTOM _NET_SUPPORTED supported' root
    pure () -- Ignore failure.

getWindowState :: Window -> X [Atom]
getWindowState win = do
    _NET_WM_STATE <- getAtom "_NET_WM_STATE"
    fold <$> getWindowProperty32 _NET_WM_STATE win

replaceWindowState :: [String] -> Window -> X ()
replaceWindowState state' win = do
    _NET_WM_STATE <- getAtom "_NET_WM_STATE"
    atoms <- traverse getAtom state'
    replaceWindowProperty32 aTOM _NET_WM_STATE atoms win
    pure () -- Ignore errors.

modifyWindowState :: ([Atom] -> [Atom]) -> Window -> X ()
modifyWindowState f win = do
    _NET_WM_STATE <- getAtom "_NET_WM_STATE"
    currentState <- getWindowState win
    replaceWindowProperty32 aTOM _NET_WM_STATE (f currentState) win
    pure () -- Ignore errors.



isFullscreen :: Query Bool
isFullscreen = ask >>= \ win -> liftX $ do
    _NET_WM_STATE_FULLSCREEN <- getAtom "_NET_WM_STATE_FULLSCREEN"
    props <- getWindowState win
    pure $ _NET_WM_STATE_FULLSCREEN `List.elem` props
