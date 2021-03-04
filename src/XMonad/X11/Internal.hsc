{-# LANGUAGE ForeignFunctionInterface #-}

module XMonad.X11.Internal where

import Graphics.X11 (Window)
import Graphics.X11.Xlib.Types (Display (..), Screen (..))

import Data.Word (Word32)
import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "XScreenCount" screenCount :: Display -> CInt

foreign import ccall unsafe "XScreenOfDisplay"
   screenOfDisplay :: Display -> CInt -> Screen

foreign import ccall "XRootWindow"
   rootWindow :: Display -> CInt -> Window

foreign import ccall unsafe "XDefaultScreen"
   defaultScreen :: Display -> CInt

defaultScreenOfDisplay :: Display -> Screen
defaultScreenOfDisplay display =
   screenOfDisplay display (defaultScreen display)

defaultRootWindow :: Display -> Window
defaultRootWindow display =
   rootWindow display (defaultScreen display)
