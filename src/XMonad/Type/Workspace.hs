
module XMonad.Type.Workspace where

import Control.Lens
import XMonad.Type.Focused
import XMonad.Core (Layout)
import Graphics.X11.Types (Window)

newtype Workspace = Workspace (String, Layout Window, Maybe (Focused Window))

mkWorkspace :: String -> Layout Window -> Maybe (Focused Window) -> Workspace
mkWorkspace n l t = Workspace (n, l, t)

name :: Lens' Workspace String
name f (Workspace (n, l, t)) = (\ n' -> mkWorkspace n' l t) <$> f n

layout :: Lens' Workspace (Layout Window)
layout f (Workspace (n, l, t)) = (\ l' -> mkWorkspace n l' t) <$> f l

tiles :: Lens' Workspace (Maybe (Focused Window))
tiles f (Workspace (n, l, t)) = mkWorkspace n l <$> f t
