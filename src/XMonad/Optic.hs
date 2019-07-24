{-# LANGUAGE
    RankNTypes
  #-}

module XMonad.Optic where

import XMonad.Core
    ( Layout (Layout)
    , Query (Query)
    , StateExtension (StateExtension)
    , WindowSet
    , WindowSpace
    , X
    , XState (XState)
    , XConf (XConf)
    , XConfig (XConfig))
import qualified XMonad.Core as XC
import XMonad.Layout
    ( Tall (Tall))
import qualified XMonad.Layout as LO
import XMonad.StackSet
    ( Stack (Stack)
    , Screen (Screen)
    , StackSet (StackSet)
    , RationalRect (RationalRect)
    , Workspace (Workspace))
import qualified XMonad.StackSet as SS
import Graphics.X11.Xlib (Display, KeyMask, Position, Window)
import Data.Map (Map)
import Data.Set (Set)
import Data.Functor.Const (Const (Const), getConst)
import Data.Functor.Identity (Identity (Identity), runIdentity)

------- Types -------

type Lens s t a b = forall m. Functor m => (a -> m b) -> s -> m t
type MonoLens s a = Lens s s a a

------- Functions -------

views l f = getConst . l (Const . f)

view l = views l id

over l f = runIdentity . l (Identity . f)

set l = over l . const

------- Lenses -------
--
----- From Core -----

--- XState ---

windowset :: MonoLens XState WindowSet 
windowset f xstate@XState{ XC.windowset = x } =
    (\ x' -> xstate{ XC.windowset = x' }) <$> f x

mapped :: MonoLens XState (Set Window)
mapped f xstate@XState{ XC.mapped = x } =
    (\ x' -> xstate{ XC.mapped = x' }) <$> f x

waitingUnmap :: MonoLens XState (Map Window Int)
waitingUnmap f xstate@XState{ XC.waitingUnmap = x } =
    (\ x' -> xstate{ XC.waitingUnmap = x' }) <$> f x

dragging :: MonoLens XState (Maybe (Position -> Position -> X (), X ()))
dragging f xstate@XState{ XC.dragging = x } =
    (\ x' -> xstate{ XC.dragging = x' }) <$> f x

numberlockMask :: MonoLens XState KeyMask
numberlockMask f xstate@XState{ XC.numberlockMask = x } =
    (\ x' -> xstate{ XC.numberlockMask = x' }) <$> f x

extensibleState :: MonoLens XState (Map String (Either String StateExtension)) 
extensibleState f xstate@XState{ XC.extensibleState = x } =
    (\ x' -> xstate{ XC.extensibleState = x' }) <$> f x

--- XConf ---

display :: MonoLens XConf Display
display f xconf@XConf{ XC.display = x } =
    (\ x' -> xconf{ XC.display = x' }) <$> f x

config :: MonoLens XConf (XConfig Layout)
config f xconf@XConf{ XC.config = x } =
    (\ x' -> xconf{ XC.config = x' }) <$> f x



----- From StackSet -----

--- Screen ----

workspace :: Lens
    (Screen    i l a sid sd) (Screen    i' l' a' sid sd)
    (Workspace i l a)        (Workspace i' l' a')
workspace f scrn@Screen{ SS.workspace = x } =
    (\ x' -> scrn{ SS.workspace = x' }) <$> f x

screen :: Lens (Screen i l a sid sd) (Screen i l a sid' sd) sid sid'
screen f scrn@Screen{ SS.screen = x } =
    (\ x' -> scrn{ SS.screen = x' }) <$> f x

screenDetail :: Lens (Screen i l a sid sd) (Screen i l a sid sd') sd sd'
screenDetail f scrn@Screen{ SS.screenDetail = x } =
    (\ x' -> scrn{ SS.screenDetail = x' }) <$> f x


--- Stack ---

focus, master :: MonoLens (Stack a) a
focus f s@Stack{ SS.focus = x } =
    (\ x' -> s{ SS.focus = x' }) <$> f x
master f s@Stack{ SS.focus = x, SS.up = xu, SS.down = xd } =
    case reverse xu of
    x' : xu' -> (\ x'' -> Stack x (reverse (x'' : xu')) xd) <$> f x'
    _ -> focus f s

up, down :: MonoLens (Stack a) [a]
up f s@Stack{ SS.up = xs } =
    (\ xs' -> s{ SS.up = xs'  }) <$> f xs
down f s@Stack{ SS.down = xs } =
    (\ xs' -> s{ SS.down = xs' }) <$> f xs



--- StackSet ---

current :: MonoLens (StackSet i l a sid sd) (Screen i l a sid sd)
current f ss@StackSet{ SS.current = x } =
    (\ x' -> ss{ SS.current = x' }) <$> f x

visible :: MonoLens (StackSet i l a sid sd) [Screen i l a sid sd]
visible f ss@StackSet{ SS.visible = x } =
    (\ x' -> ss{ SS.visible = x' }) <$> f x

hidden :: MonoLens (StackSet i l a sid sd) [Workspace i l a]
hidden f ss@StackSet{ SS.hidden = x } =
    (\ x' -> ss{ SS.hidden = x' }) <$> f x

floating :: MonoLens (StackSet i l a sid sd) (Map a RationalRect)
floating f ss@StackSet{ SS.floating = x } =
    (\ x' -> ss{ SS.floating = x' }) <$> f x


--- Workspace ---

tag :: Lens (Workspace i l a) (Workspace i' l a) i i'
tag f ws@Workspace{ SS.tag = x } =
    (\ x' -> ws{ SS.tag = x' }) <$> f x

layout :: Lens (Workspace i l a) (Workspace i l' a) l l'
layout f ws@Workspace{ SS.layout = x } =
    (\ x' -> ws{ SS.layout = x' }) <$> f x

stack :: Lens
    (Workspace i l a) (Workspace i l a')
    (Maybe (Stack  a)) (Maybe (Stack a'))
stack f ws@Workspace{ SS.stack = x } =
    (\ x' -> ws{ SS.stack = x' }) <$> f x

