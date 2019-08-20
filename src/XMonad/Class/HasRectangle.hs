
module XMonad.Class.HasRectangle where

import Control.Lens
import Graphics.X11.Xlib (Rectangle)



class HasRectangle a where
    _Rectangle :: Lens' a Rectangle
