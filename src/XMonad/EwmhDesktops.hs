
module XMonad.EwmhDesktops (
    ewmh,
    ewmhDesktopStartup,
    ewmhDesktopsLogHook,
    ewmhDesktopsEventHook,
    activated, activateLogHook,
    fullscreenEventHook,
    ) where

import qualified Codec.Binary.UTF8.String as UTF8

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Control.Monad.State as State

import Data.Foldable (fold, for_, traverse_)
import qualified Data.Bits as Bits
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup (All (All, getAll), appEndo)

import qualified Foreign.C.Types as C

import XMonad
    ( ExtensionClass (initialValue)
    , Atom, Event (..), ManageHook, Query, WorkspaceId, Window, WindowSpace, X, XConfig
    , liftX, runQuery, windows
    , config, display, theRoot, handleEventHook, logHook, startupHook, windowset
    , killWindow, trace
    , none
    , changeProperty8, getWindowProperty32, changeProperty32
    , internAtom, aTOM, cARDINAL, wINDOW, propModeReplace
    )
import qualified XMonad.StackSet as W
import qualified XMonad.ExtensibleState as ES


data EwmhCache = EwmhCache
    { desktopNames :: ![String]
    , clientList :: ![Window]
    , currentDesktop :: !Int
    , windowDesktops :: !(Map Window C.CLong)
    , activeWindow :: !Window
    , netActivated :: !(Maybe Window)
    , atomCache :: !(Map String Atom)
    }

instance ExtensionClass EwmhCache where
    initialValue =
        EwmhCache [] [] 0 Map.empty (Bits.complement none) Nothing Map.empty

ewmh :: XConfig a -> XConfig a
ewmh cfg = cfg
    { startupHook = ewmhDesktopStartup <> startupHook cfg
    , handleEventHook = fullscreenEventHook <> ewmhDesktopsEventHook <> handleEventHook cfg
    , logHook = ewmhDesktopsLogHook <> logHook cfg
    }

ewmhDesktopStartup :: X ()
ewmhDesktopStartup = setSupported' (
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
    [])

ewmhDesktopsLogHook :: X ()
ewmhDesktopsLogHook = do
    s <- State.gets windowset
    let ws = W.workspaces s

    let
      desktop_names :: [WorkspaceId]
      desktop_names = map W.tag ws
    setNumberOfDesktops (length desktop_names)
    setDesktopNames desktop_names

    let client_list = List.nub . foldMap (W.integrate' . W.stack) $ ws
    setClientList client_list

    let maybeCurrent = W.tag <$> W.workspace . W.current $ s
        current = (`List.elemIndex` fmap W.tag ws) maybeCurrent
    traverse_ setCurrentDesktop current

    let
      f wsId workspace =
          Map.fromList [ (winId, wsId) | winId <- W.integrate' $ W.stack workspace ]
      window_desktops = Map.unions $ zipWith f [0..] ws
    traverse_ (uncurry setWindowDesktop) (Map.toList window_desktops)

    let activeWindow' = fromMaybe none (W.peek s)
    setActiveWindow activeWindow'

    es <- ES.get
    ES.put es
        { windowDesktops = window_desktops
        , desktopNames = desktop_names
        , clientList = client_list
        , currentDesktop = fromMaybe (currentDesktop es) current
        , activeWindow = activeWindow'
        }

activated :: Query Bool
activated = liftX $ ES.gets $ isJust . netActivated

activateLogHook :: ManageHook -> X ()
activateLogHook hook = do
    es <- ES.get
    netActivated es `for_` \ w -> do
        f <- runQuery hook w
        ES.put $ es{ netActivated = Nothing }
        windows $ appEndo f

ewmhDesktopsEventHook :: Event -> X All
ewmhDesktopsEventHook e = handle id e *> pure (All True)

handle :: ([WindowSpace] -> [WindowSpace]) -> Event -> X ()
handle f msg@ClientMessageEvent{} = do
    ws <- State.gets $ f . W.workspaces . windowset

    _NET_CURRENT_DESKTOP <- getAtom "_NET_CURRENT_DESKTOP"
    _NET_WM_DESKTOP <- getAtom "_NET_WM_DESKTOP"
    _NET_ACTIVE_DESKTOP <- getAtom "_NET_ACTIVE_DESKTOP"
    _NET_CLOSE_WINDOW <- getAtom "_NET_CLOSE_WINDOW"
    case ev_message_type msg of
        mt
            | mt == _NET_CURRENT_DESKTOP ->
                case ev_data msg of
                    n : _ | 0 <= n  &&  fromIntegral n < length ws ->
                        windows $ W.view $ W.tag $ ws !! fromIntegral n
                    _ ->
                        trace $ "Bad _NET_CURRENT_DESKTOP data"
            | mt == _NET_WM_DESKTOP ->
                case ev_data msg of
                    n : _ | 0 <= n && fromIntegral n < length ws ->
                        windows $ W.shiftWin (W.tag $ ws !! fromIntegral n) (ev_window msg)
                    _ ->
                        trace $ "Bad _NET_CURRENT_DESKTOP data"
            | mt == _NET_ACTIVE_DESKTOP -> do
                lh <- asks $ logHook . config
                es <- ES.get
                ES.put es{ netActivated = Just (ev_window msg)}
                lh
            | mt == _NET_CLOSE_WINDOW -> killWindow (ev_window msg)
            | otherwise -> pure ()
handle _ _ = pure ()

fullscreenEventHook :: Event -> X All
fullscreenEventHook ClientMessageEvent
    { ev_event_display = dpy
    , ev_window = win
    , ev_event_type = typ
    , ev_data = (act : dats)
    } = do
    _NET_WM_STATE <- getAtom "_NET_WM_STATE"
    _NET_WM_STATE_FULLSCREEN <- getAtom "_NET_WM_FULLSCREEN"
    wstate <- liftIO $ fold <$> getWindowProperty32 dpy _NET_WM_STATE win

    let
      isFull = fromIntegral _NET_WM_STATE_FULLSCREEN `elem` wstate
      changeWindowState f = liftIO $ changeProperty32 dpy win _NET_WM_STATE aTOM propModeReplace (f wstate)

    when (typ == fromIntegral _NET_WM_STATE  &&  fromIntegral _NET_WM_STATE_FULLSCREEN `elem` dats) $ do
        when (act == add  || (act == toggle  &&  not isFull)) $ do
            changeWindowState (fromIntegral _NET_WM_STATE_FULLSCREEN :)
            windows $ W.float win $ W.RationalRect 0 0 1 1
        when (act == remove  ||  (act == toggle  &&  isFull)) $ do
            changeWindowState $ List.delete $ fromIntegral _NET_WM_STATE_FULLSCREEN
            windows $ W.sink win
    pure $ All True
  where
    remove = 0
    add = 1
    toggle = 2
fullscreenEventHook _ = pure $ All True

setActiveWindow :: Window -> X ()
setActiveWindow w = do
    dpy <- asks display
    r <- asks theRoot
    _NET_ACTIVE_WINDOW <- getAtom "_NET_ACTIVE_WINDOW"
    liftIO $ changeProperty32 dpy r _NET_ACTIVE_WINDOW wINDOW propModeReplace [fromIntegral w]

setNumberOfDesktops :: Int -> X ()
setNumberOfDesktops n = do
    dpy <- asks display
    r <- asks theRoot
    a <- getAtom "_NET_NUMBER_OF_DESKTOPS"
    liftIO $ changeProperty32 dpy r a cARDINAL propModeReplace [fromIntegral n]

setCurrentDesktop :: Int -> X ()
setCurrentDesktop i = do
    dpy <- asks display
    r <- asks theRoot
    a <- getAtom "_NET_CURRENT_DESKTOP"
    liftIO $ changeProperty32 dpy r a cARDINAL propModeReplace [fromIntegral i]

setDesktopNames :: (Foldable m)=> m String -> X ()
setDesktopNames names = do
   dpy <- asks display
   r <- asks theRoot
   a <- getAtom "_NET_DESKTOP_NAMES"
   c <- getAtom "UTF8_STRING"
   let names' = fromIntegral <$> foldMap ((<> [0]) . UTF8.encode) names
   liftIO $ changeProperty8 dpy r a c propModeReplace names'

setClientList :: [Window] -> X ()
setClientList clients = do
    dpy <- asks display
    r <- asks theRoot
    a <- getAtom "_NET_CLIENT_LIST"
    liftIO $ changeProperty32 dpy r a wINDOW propModeReplace (fmap fromIntegral clients)
    a' <- getAtom "_NET_CLIENT_LIST_STACKING"
    liftIO $ changeProperty32 dpy r a' wINDOW propModeReplace (fmap fromIntegral clients)

setWindowDesktop :: Window -> C.CLong -> X ()
setWindowDesktop win i = do
    dpy <- asks display
    a <- getAtom "_NET_WM_DESKTOP"
    liftIO $ changeProperty32 dpy win a cARDINAL propModeReplace [i]

addSupported :: [String] -> X ()
addSupported properties = do
    newSupported <- traverse (fmap fromIntegral . getAtom) properties
    modifySupported $ List.union newSupported

modifySupported :: ([C.CLong] -> [C.CLong]) -> X ()
modifySupported f = do
   disp <- asks display
   root <- asks theRoot
   _NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
   liftIO $ do
   oldSupported <- getWindowProperty32 disp _NET_SUPPORTED root
   changeProperty32 disp root _NET_SUPPORTED aTOM propModeReplace $ f $ fold oldSupported

setSupported' :: [String] -> X ()
setSupported' supported = do
   disp <- asks display
   root <- asks theRoot
   _NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
   supported' <- traverse (fmap fromIntegral . getAtom) supported
   liftIO $ changeProperty32 disp root _NET_SUPPORTED aTOM propModeReplace supported'

getAtom :: String -> X Atom
getAtom string = do
    disp <- asks display
    es <- ES.get
    case Map.lookup string (atomCache es) of
        Just atom -> pure atom
        Nothing -> do
            atom <- liftIO $ internAtom disp string False
            ES.put $ es{ atomCache = Map.insert string atom (atomCache es) }
            pure atom
