{-# LANGUAGE ScopedTypeVariables
   #-}

{- | This module provides a class for environments that contain an 'X.Display', and a number of fuctions for getting and setting X11 window properties and 'X.Atom's.

One thing we do here is try to pretend that root windows don't exist. So functions that operate on root windows instead operate on screens.
-}

module XMonad.X11 (
   HasDisplay (..),
   Strut (..),
   getAtom, getWindowProperty, setWindowProperty,
   setActiveWindow, setClientLists, setDesktopNames, setNumberOfDesktops,
   getSupported, setSupported, addSupported,
   getWMNormalHints,
   setWindowAllowedActions,
   getWindowDesktop, setWindowDesktop,
   getWindowName, setWindowVisibleName,
   getWindowType,
   getWindowState, setWindowState,
   getWindowStrut,
   _NET_ACTIVE_WINDOW, _NET_CLIENT_LIST, _NET_CLIENT_LIST_STACKING, _NET_CURRENT_DESKTOP, _NET_NUMBER_OF_DESKTOPS,
   ) where

import qualified Control.Exception as Err
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader

import Data.Foldable (traverse_)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import qualified System.IO.Unsafe as Unsafe

import Foreign.C.Types (CInt, CLong)
import Foreign.Storable (Storable)

import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as X

import qualified XMonad.X11.IO as IO
import qualified XMonad.X11.Internal as Internal
import XMonad.X11.IO (Strut)

class HasDisplay a where
{- ^
Something with a reference to an 'X.Display'
Having a display means that you have access to the entire X11 state.
-}
   display :: a -> X.Display

   screenCount :: a -> CInt
   screenCount = Internal.screenCount . display

   screenOfDisplay :: a -> CInt -> X.Screen
   screenOfDisplay = Internal.screenOfDisplay . display

   rootWindow :: a -> CInt -> X.Window
   rootWindow = Internal.rootWindow . display

   defaultScreen :: a -> CInt
   defaultScreen = Internal.defaultScreen . display

instance HasDisplay X.Display where
   display = id

askScreenCount ::
   (HasDisplay r, MonadReader r m)=> m CInt
askScreenCount = Reader.asks screenCount

askScreen ::
   (HasDisplay r, MonadReader r m)=> CInt -> m X.Screen
askScreen n = Reader.asks (\ d -> screenOfDisplay d n)

askScreens ::
   (HasDisplay r, MonadReader r m)=> m [X.Screen]
askScreens = Reader.asks (\ d -> screenOfDisplay d <$> [0 .. screenCount d - 1])

askDefaultScreen ::
   (HasDisplay r, MonadReader r m)=> m CInt
askDefaultScreen = Reader.asks defaultScreen

askDefaultRootWindow ::
   (HasDisplay r, MonadReader r m)=> m (X.Window)
askDefaultRootWindow = Reader.asks (\ d -> rootWindow d (defaultScreen d))

withAskDisplayIO ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   (X.Display -> IO a) -> m a
withAskDisplayIO act = Reader.asks display >>= liftIO . act
{-# INLINE withAskDisplayIO #-}

getWMNormalHints ::
   (MonadIO m, MonadReader a m, HasDisplay a)=>
   X.Window -> m X.SizeHints
getWMNormalHints = withAskDisplayIO . flip X.getWMNormalHints
{-# INLINE getWMNormalHints #-}

getWindowProperty ::
   (MonadIO m, MonadReader d m, HasDisplay d)=>
   forall a. (Storable a)=>
   String -> X.Window -> m (Maybe [a])
getWindowProperty property window = withAskDisplayIO $ \ d ->
   IO.getWindowProperty d property window
{-# INLINE getWindowProperty #-}

setWindowProperty ::
   (MonadIO m, MonadReader d m, HasDisplay d)=>
   forall a. (Storable a)=>
   X.Atom -> String -> [a] -> X.Window -> m X.Status
setWindowProperty property_type property value window = withAskDisplayIO $ \ d -> do
   IO.setWindowProperty d property_type property value window
{-# INLINE setWindowProperty #-}

getAtom ::
   (MonadIO m, MonadReader a m, HasDisplay a)=>
   String -> m X.Atom
{- ^ Retrieve the atom corresponding to the provided string. This first checks whether the atom has been cached, and if so, gets it from the cache. Otherwise it gets it the display and puts it in the cache.
-}
getAtom = withAskDisplayIO . flip IO.getAtom
{-# INLINE getAtom #-}


--- Window Manager (root window) Properties ---

-- setActiveWindow ::
--    (HasDisplay r, MonadReader r m, MonadIO m)=>
--    X.Window -> X.Window -> m X.Status
-- setActiveWindow window root_window = withAskDisplayIO $ \ d ->
--    IO.setActiveWindow d window root_window
setActiveWindow ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   CInt -> X.Window -> m X.Status
setActiveWindow screen window = withAskDisplayIO $ \ d ->
   IO.setActiveWindow d window (rootWindow d screen)

-- setClientLists clients root_window = withAskDisplayIO $ \ d ->
--    IO.setClientLists d clients root_window
setClientLists ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   CInt -> [X.Window] -> m X.Status
setClientLists screen clients = withAskDisplayIO $ \ d ->
   IO.setClientLists d (rootWindow d screen) clients

setDesktopNames ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   CInt -> [String] -> m X.Status
setDesktopNames screen names = withAskDisplayIO $ \ d ->
   IO.setDesktopNames d (rootWindow d screen) names

setNumberOfDesktops ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   CInt -> Word32 -> m X.Status
setNumberOfDesktops screen number = withAskDisplayIO $ \ d ->
   IO.setNumberOfDesktops d (rootWindow d screen)  number

getSupported ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   X.Window -> m (Maybe [X.Atom])
getSupported = withAskDisplayIO . flip IO.getSupported

setSupported ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   CInt -> [String] -> m X.Status
setSupported screen supported = withAskDisplayIO $ \ d ->
   IO.setSupported d (rootWindow d screen) supported

addSupported ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   CInt -> [String] -> m X.Status
addSupported screen supported = withAskDisplayIO $ \ d ->
   IO.addSupported d (rootWindow d screen) supported

addSupportedAll ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   [String] -> m ()
addSupportedAll supported = do
   n <- askScreenCount
   traverse_ (`addSupported` supported) [0 .. n - 1]

--- Application Window Properties ---

setWindowAllowedActions ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   [String] -> X.Window -> m X.Status
setWindowAllowedActions allowed window = withAskDisplayIO $ \ d ->
   IO.setWindowAllowedActions d allowed window

getWindowDesktop ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   X.Window -> m (Maybe Word32)
getWindowDesktop = withAskDisplayIO . flip IO.getWindowDesktop

setWindowDesktop ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   Word32-> X.Window -> m X.Status
setWindowDesktop desktop window = withAskDisplayIO $ \ d ->
   IO.setWindowDesktop d desktop window

getWindowName ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   X.Window -> m (Maybe String)
getWindowName = withAskDisplayIO . flip IO.getWindowName
{-# INLINE getWindowName #-}

setWindowVisibleName ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   String -> X.Window -> m X.Status
setWindowVisibleName name window = withAskDisplayIO $ \ d ->
   IO.setWindowVisibleName d name window
{-# INLINE setWindowVisibleName #-}

getWindowType ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   X.Window -> m (Maybe [X.Atom])
getWindowType = withAskDisplayIO . flip IO.getWindowType
{-# INLINE getWindowType #-}

getWindowState ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   X.Window -> m (Maybe [X.Atom])
getWindowState = withAskDisplayIO . flip IO.getWindowState
{-# INLINE getWindowState #-}

setWindowState ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   [String] -> X.Window -> m X.Status
setWindowState status window = withAskDisplayIO $ \ d ->
   IO.setWindowState d status window
{-# INLINE setWindowState #-}

getWindowStrut ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   X.Window -> m (Maybe Strut)
{- ^ Try to get the strut property of a Window. -}
getWindowStrut = withAskDisplayIO . flip IO.getWindowStrut
{-# INLINE getWindowStrut #-}

--- Window Manager Property Atoms ---

_NET_ACTIVE_WINDOW ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_ACTIVE_WINDOW = getAtom "_NET_ACTIVE_WINDOW"

_NET_CLIENT_LIST ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_CLIENT_LIST = getAtom "_NET_CLIENT_LIST"

_NET_CLIENT_LIST_STACKING ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_CLIENT_LIST_STACKING = getAtom "_NET_CLIENT_LIST_STACKING"

_NET_CURRENT_DESKTOP ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_CURRENT_DESKTOP = getAtom "_NET_CURRENT_DESKTOP"

_NET_NUMBER_OF_DESKTOPS ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_NUMBER_OF_DESKTOPS = getAtom "_NET_NUMBER_OF_DESKTOPS"


-- Application Window Property Atoms

_NET_WM_STRUT ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STRUT = getAtom "_NET_WM_STRUT"

_NET_WM_STRUT_PARTIAL ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STRUT_PARTIAL = getAtom "_NET_WM_STRUT_PARTIAL"


-- Window State Atoms

_NET_WM_STATE_MODAL ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_MODAL = getAtom "_NET_WM_STATE_MODAL"

_NET_WM_STATE_MAXIMIZED_VERT ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_MAXIMIZED_VERT = getAtom "_NET_WM_STATE_MAXIMIZED_VERT"

_NET_WM_STATE_MAXIMIZED_HORZ ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_MAXIMIZED_HORZ = getAtom "_NET_WM_STATE_MAXIMIZED_HORZ"

_NET_WM_STATE_HIDDEN ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_HIDDEN = getAtom "_NET_WM_STATE_HIDDEN"

_NET_WM_STATE_FULLSCREEN ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_FULLSCREEN = getAtom "_NET_WM_STATE_FULLSCREEN"

_NET_WM_STATE_ABOVE ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
{- ^ This indicates that the window may be placed in the "above" layer of windows.
-}
_NET_WM_STATE_ABOVE = getAtom "_NET_WM_STATE_ABOVE"

_NET_WM_STATE_BELOW ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
{- ^ This indicates that the window may be placed in the "below" layer of windows.
-}
_NET_WM_STATE_BELOW = getAtom "_NET_WM_STATE_BELOW"

_NET_WM_STATE_DEMANDS_ATTENTION ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_DEMANDS_ATTENTION = getAtom "_NET_WM_STATE_DEMANDS_ATTENTION"


-- Allowed Window Action Atoms

_NET_WM_ACTION_ABOVE ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
{- ^ This indicates that the window will respond to changes in '_NET_WM_STATE_ABOVE'. -}
_NET_WM_ACTION_ABOVE = getAtom "_NET_WM_ACTION_ABOVE"

_NET_WM_ACTION_BELOW ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
{- ^ This indicates that the window will respond to changes in '_NET_WM_STATE_BELOW'. -}
_NET_WM_ACTION_BELOW = getAtom "_NET_WM_ACTION_BELOW"

_NET_WM_ACTION_CLOSE ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
{- ^ This indicates that the window will respond to '_NET_CLOSE_WINDOW' messages. -}
_NET_WM_ACTION_CLOSE = getAtom "_NET_WM_ACTION_CLOSE"

_NET_WM_ACTION_RESIZE ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
{- ^ This indicates that the window can be resized. -}
_NET_WM_ACTION_RESIZE = getAtom "_NET_WM_ACTION_RESIZE"

_NET_WM_ACTION_FULLSCREEN ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_ACTION_FULLSCREEN = getAtom "_NET_WM_ACTION_FULLSCREEN"



{- Notes

# Inlining
Monadic functions that are polymorphic in their monad should be inlined.

Some properties that it only makes sense for XMonad to set, not get. For example, _NET_NUMBER_OF_DESKTOPS


-}
