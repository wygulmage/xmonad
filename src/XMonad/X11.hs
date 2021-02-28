{-# LANGUAGE BangPatterns
           , ScopedTypeVariables
   #-}

module XMonad.X11 (
   HasDisplay (..),
   Strut (..), getWindowStrut,
   getWMNormalHints,
   getAtom,
   getWindowProperty, setWindowProperty,
   _NET_ACTIVE_WINDOW, _NET_CLIENT_LIST, _NET_CLIENT_LIST_STACKING, _NET_CURRENT_DESKTOP, _NET_NUMBER_OF_DESKTOPS,
   ) where

import qualified Control.Exception as Err
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader

import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import qualified System.IO.Unsafe as Unsafe

import Foreign.C.Types (CLong)
import Foreign.Storable (Storable)

import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as X

import qualified XMonad.X11.IO as IO
import XMonad.X11.IO (Strut)

class HasDisplay a where
   display :: a -> X.Display

instance HasDisplay X.Display where
   display = id

getWindowStrut ::
   (HasDisplay r, MonadReader r m, MonadIO m)=>
   X.Window -> m (Maybe Strut)
{- ^ Try to get the strut property of a Window. -}
getWindowStrut = withAskDisplayIO . flip IO.getWindowStrut
{-# INLINE getWindowStrut #-}


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


-- Window State Atoms
_NET_WM_STATE_MODAL ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_MODAL = getAtom "_NET_WM_STATE_MODAL"

_NET_WM_STATE_STICKY ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_STICKY = getAtom "_NET_WM_STATE_STICKY"

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
_NET_WM_STATE_ABOVE = getAtom "_NET_WM_STATE_ABOVE"

_NET_WM_STATE_BELOW ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_BELOW = getAtom "_NET_WM_STATE_BELOW"

_NET_WM_STATE_DEMANDS_ATTENTION ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STATE_DEMANDS_ATTENTION = getAtom "_NET_WM_STATE_DEMANDS_ATTENTION"

-- Allowed Window Action Atoms
_NET_WM_ACTION_RESIZE ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_ACTION_RESIZE = getAtom "_NET_WM_ACTION_RESIZE"

_NET_WM_ACTION_FULLSCREEN ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_ACTION_FULLSCREEN = getAtom "_NET_WM_ACTION_FULLSCREEN"

_NET_WM_ACTION_CLOSE ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_ACTION_CLOSE = getAtom "_NET_WM_ACTION_CLOSE"

--
_NET_WM_STRUT ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STRUT = getAtom "_NET_WM_STRUT"

_NET_WM_STRUT_PARTIAL ::
   (MonadIO m, MonadReader a m, HasDisplay a)=> m X.Atom
_NET_WM_STRUT_PARTIAL = getAtom "_NET_WM_STRUT_PARTIAL"




{- Notes

# Inlining
Monadic functions that are polymorphic in their monad should be inlined.

Some properties that it only makes sense for XMonad to set, not get. For example, _NET_NUMBER_OF_DESKTOPS


-}
