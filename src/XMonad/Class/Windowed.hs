module XMonad.Class.Windowed where


import Control.Lens
import Graphics.X11.Xlib (Window)


class Windowed a where
    _windows :: Traversal' a Window
