{-# LANGUAGE MultiWayIf
           , PatternGuards
           , ScopedTypeVariables #-}

module XMonad.EwmhDesktops (
    ewmh,
    ewmhDesktopStartup,
    ewmhDesktopsLogHook,
    ewmhDesktopsEventHook,
    fullscreenEventHook,
    ) where

import qualified Codec.Binary.UTF8.String as UTF8

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Control.Monad.State as State

import Data.Foldable (fold, foldl', for_, traverse_)
import qualified Data.Bits as Bits
import Data.Int (Int32)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup (All (All), appEndo)
import Data.Word (Word32)

import qualified Foreign.C.Types as C
import qualified Foreign.Marshal.Alloc as Storable
import qualified Foreign.Marshal.Array as Storable
import qualified Foreign.Ptr as Ptr
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable

import XMonad
    ( ExtensionClass (initialValue)
    , Atom, Display, Event (..), ManageHook, Query, WorkspaceId, Window, WindowSpace, X, XConfig
    , liftX, runQuery, windows
    , config, display, theRoot, handleEventHook, logHook, startupHook, windowset
    , killWindow, trace
    , none
    , internAtom, aTOM, cARDINAL, wINDOW, propModeReplace
    )
import qualified XMonad.StackSet as W
import qualified XMonad.ExtensibleState as ES

import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as X11


data EwmhCache = EwmhCache
    { desktopNames :: ![String]
    , clientList :: ![Window]
    , currentDesktop :: !Int32
    , windowDesktops :: ![[Window]]
    , activeWindow :: !Window
    , atomCache :: !(Map String Atom)
    }

instance ExtensionClass EwmhCache where
    initialValue = EwmhCache
        { desktopNames = []
        , clientList = []
        , currentDesktop = 0
        , windowDesktops = []
        , activeWindow = none
        , atomCache = Map.empty
        }


ewmh :: XConfig a -> XConfig a
ewmh cfg = cfg
    { startupHook = ewmhDesktopStartup <> startupHook cfg
    , handleEventHook = fullscreenEventHook <> ewmhDesktopsEventHook <> handleEventHook cfg
    , logHook = ewmhDesktopsLogHook <> logHook cfg
    }

ewmhDesktopStartup :: X ()
ewmhDesktopStartup = setSupported ( -- This presumes that nothing else is modifying the supported list.
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
{- ^ Update EWMH state.
-}
ewmhDesktopsLogHook = do
    es <- ES.get
    s <- State.gets windowset
    let ws = List.sortOn W.tag $ W.workspaces s

    let
      desktop_names :: [WorkspaceId]
      desktop_names = fmap W.tag ws
    setNumberOfDesktops $ intToInt32 $ length desktop_names
    when (desktopNames es /= desktop_names) $ setDesktopNames desktop_names

    let
      client_list = foldl' (\ cs c -> List.union cs $ W.integrate' $ W.stack c) [] ws
    when (clientList es /= client_list) $ setClientList client_list


    let
      current = fmap intToInt32 $
          (W.tag . W.workspace . W.current) s `List.elemIndex` desktop_names
    when (Just (currentDesktop es) /= current) $
        for_ current (setCurrentDesktop)

    let
      window_desktops = W.integrate' . W.stack <$> ws
      itraverse_ f = loop 0
        where
        loop i xs = case xs of
            x : xs' -> f i x *> loop (i + 1) xs'
            []      -> pure ()
    when (windowDesktops es /= window_desktops) $
        itraverse_ (traverse_ . setWindowDesktop) window_desktops

    let activeWindow' = fromMaybe none $ W.peek s
    when (activeWindow es /= activeWindow') $ setActiveWindow activeWindow'

    ES.put es
        { windowDesktops = window_desktops
        , desktopNames = desktop_names
        , clientList = client_list
        , currentDesktop = maybe (currentDesktop es) fromIntegral current
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
            | mt == _NET_CURRENT_DESKTOP ->
                case ev_data msg of
                    n : _ | n' <- cIntToInt n
                          , 0 <= n'  &&  n' < length ws ->
                        windows . W.view . W.tag $ ws !! n'
                    _ ->
                        trace "Bad _NET_CURRENT_DESKTOP data"
            | mt == _NET_WM_DESKTOP ->
                case ev_data msg of
                    n : _ | n' <- cIntToInt n
                          , 0 <= n' && n' < length ws ->
                        windows . W.shiftWin (W.tag $ ws !! n') $ ev_window msg
                    _ ->
                        trace "Bad _NET_CURRENT_DESKTOP data"
            | mt == _NET_ACTIVE_WINDOW -> do -- FIXME: User config should decide how to honor client window activation requests.
                  windows . W.focusWindow $ ev_window msg
                  setActiveWindow $ ev_window msg
            | mt == _NET_CLOSE_WINDOW -> killWindow $ ev_window msg
            | otherwise -> pure ()
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

        wstate <- fold <$> getWindowProperty _NET_WM_STATE win
        let
          isFull = _NET_WM_STATE_FULLSCREEN_32 `elem` wstate
          changeWindowState f = replaceWindowProperty aTOM _NET_WM_STATE (f wstate) win
        if
            | act == add || (act == toggle  &&  not isFull) -> do
                changeWindowState (_NET_WM_STATE_FULLSCREEN_32 :)
                windows $ W.float win $ W.RationalRect 0 0 1 1
            | act == remove || (act == toggle && not isFull) -> do
                changeWindowState $ List.delete $ _NET_WM_STATE_FULLSCREEN_32
                windows $ W.sink win
            | otherwise -> pure ()
  where
    remove = 0
    add = 1
    toggle = 2
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


--- INTERNAL ---

intToInt32 :: Int -> Int32
intToInt32 = fromIntegral

atomToWord32 :: Atom -> Word32
atomToWord32 = fromIntegral

word32ToAtom :: Word32 -> Atom
word32ToAtom = fromIntegral

atomToCInt :: Atom -> C.CInt
atomToCInt = fromIntegral

windowToWord32 :: Window -> Word32
windowToWord32 = fromIntegral

cIntToInt :: C.CInt -> Int
cIntToInt = fromIntegral

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

getWindowProperty :: (Storable a)=> Atom -> Window -> X (Either C.CInt [a])
getWindowProperty prop win = do
    disp <- asks display
    liftIO $ getWindowPropertyIO disp prop win


getWindowPropertyIO ::
    forall a. (Storable a)=>
    Display -> Atom -> Window -> IO (Either C.CInt [a])
{- ^ Get a window property. It's important to get the sizes of your types right; Use 32-bit types to get 32-bit properties. (Atom, Window, &c. are currently (2021-03-08) 64 bits on 64-bit systems.)
-}
getWindowPropertyIO disp prop win =
    -- Note: XFree is for freeing objects that were allocated by Xlib; here the objects are allocated by GHC.
    Storable.alloca $ \ actual_type_return ->
    Storable.alloca $ \ actual_format_return ->
    Storable.alloca $ \ nitems_return ->
    Storable.alloca $ \ bytes_after_return ->
    Storable.alloca $ \ prop_return -> do
        result <- X11.xGetWindowProperty
            disp
            win
            prop
            0                   -- Start at the beginning.
            0xFFFFFFFF          -- Get up to about 17 gigabytes.
            False
            X11.anyPropertyType
            actual_type_return
            actual_format_return
            nitems_return
            bytes_after_return
            prop_return

        if result /= X11.success
            then pure $ Left result
            else do
                actual_format  <- Storable.peek actual_format_return
                if actual_format /= bits
                    then pure $ Left X11.badValue
                    else do
                        nitems <- cuLongToInt <$> Storable.peek nitems_return
                        val_ptr <- Ptr.castPtr <$> Storable.peek prop_return
                        value <- Storable.peekArray nitems val_ptr
                        pure $ Right value
  where
    bits :: C.CInt
    bits = fromIntegral $ 8 * Storable.sizeOf (undefined :: a)
    cuLongToInt :: C.CULong -> Int
    cuLongToInt = fromIntegral

replaceWindowProperty ::
    (Storable a)=> Atom -> Atom -> [a] -> Window -> X C.CInt
replaceWindowProperty typ prop val win = do
    disp <- asks display
    liftIO $ replaceWindowPropertyIO disp typ prop val win

replaceWindowPropertyIO ::
    forall a. (Storable a)=>
    Display -> Atom -> Atom -> [a] -> Window -> IO C.CInt
replaceWindowPropertyIO disp typ prop val win =
    Storable.withArrayLen val $ \ len ptr ->
        X11.xChangeProperty
            disp
            win
            prop
            typ
            (fromIntegral $ 8 * Storable.sizeOf (undefined :: a))
            propModeReplace
            (Ptr.castPtr ptr)
            (fromIntegral len)
