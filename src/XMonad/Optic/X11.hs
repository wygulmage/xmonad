{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  #-}

module XMonad.Optic.X11
    ( HasPosition (..), MayHavePosition (..), Position, position
    , HasDimensions (..), MayHaveDimensions (..), Dimensions (..)
    , HasBorderWidth (..), MayHaveBorderWidth (..)
    , MayHaveRoot (..), MayHaveRootPosition (..)
    , HasSerial (..)
    , HasEventType (..)
    , RootPosition (..)
    , Event
    , WindowAttributes (WindowAttributes), mapInstalled, mapState
    , WindowChanges (WindowChanges), sibling, stackMode
    ) where

import Control.Lens
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Foreign.C.Types (CInt, CULong)
import Graphics.X11.Types (Colormap, EventType, NotifyDetail, Window)
import Graphics.X11.Xlib (Display)
import qualified Graphics.X11.Xlib as X
import Graphics.X11.Xlib.Extras (Event, WindowAttributes (WindowAttributes), WindowChanges (WindowChanges))
import qualified Graphics.X11.Xlib.Extras as X
import Data.Coerce (coerce)

-- data Position = Position {-# UNPACK #-} !CInt {-# UNPACK #-} !CInt
-- data Dimensions = Dimensions {-# UNPACK #-} !CInt {-# UNPACK #-} !CInt
-- data RootPosition = RootPosition {-# UNPACK #-} !Window {-# UNPACK #-} !CInt {-# UNPACK #-} !CInt
newtype Position = Position{ getPosition :: (CInt, CInt) }
position :: Integral a => Iso' Position (a, a)
position = iso
    (join bimap fromIntegral . getPosition)
    (Position . join bimap fromIntegral)

newtype Dimensions = Dimensions{ getDimensions :: (CInt, CInt) }
dimensions :: Integral a => Iso' Dimensions (a, a)
dimensions = iso
    (join bimap fromIntegral . getDimensions)
    (Dimensions . join bimap fromIntegral)

newtype RootPosition = RootPosition (Window, Position)
rootPosition :: Iso' RootPosition (Window, Position)
rootPosition = iso coerce RootPosition

----- Lens Classes -----
--- NOTE: 'x' and 'y' are in scope unqualified as methods. Don't use them as variables!

class HasPosition t where
    x :: Lens' t CInt
    y :: Lens' t CInt

instance HasPosition Position where
    x = position . _1
    y = position . _2

instance HasPosition RootPosition where
    x = rootPosition . _2 . x
    y = rootPosition . _2 . y

class HasDimensions t where
    height :: Lens' t CInt
    width :: Lens' t CInt

instance HasDimensions Dimensions where
    height = dimensions . _1
    width = dimensions . _2

class HasBorderWidth t where
    borderWidth :: Lens' t CInt

class HasColormap t where
    colormap :: Lens' t Colormap

class HasOverrideRedirect t where
    overrideRedirect :: Lens' t Bool

class HasSerial t where
    serial :: Lens' t CULong

class HasEventType t where
    eventType :: Lens' t EventType

class HasSendEvent t where
    sendEvent :: Lens' t Bool

class HasEventDisplay t where
    eventDisplay :: Lens' t Display

class HasWindow t where
    window :: Lens' t Window

instance HasWindow RootPosition where
    window = rootPosition . _1



----- Affine Traversal Classes -----

class MayHavePosition t where
    _Position :: Traversal' t Position

instance MayHavePosition RootPosition where
    _Position = rootPosition . _2

class MayHaveDimensions t where
    _Dimensions :: Traversal' t Dimensions

class MayHaveAbove t where
    _above :: Traversal' t Window

class MayHaveBorderWidth t where
    _borderWidth :: Traversal' t CInt

class MayHaveDetail t where
    _detail :: Traversal' t NotifyDetail

class MayHaveEvent t where
    _event :: Traversal' t Window

class MayHaveKeycode t where
    _keycode :: Traversal' t Window

class MayHaveOverrideRedirect t where
    _overrideRedirect :: Traversal' t Bool

class MayHaveParent t where
    _parent :: Traversal' t Window

class MayHaveRoot t where
    _root :: Traversal' t Window

class MayHaveRoot t => MayHaveRootPosition t where
    _RootPosition :: Traversal' t RootPosition
    -- Gives the window and the position.

class MayHaveSubwindow t where
    _subwindow :: Traversal' t Window

----- Event -----

--- Every Event has an eventType, a sendEvent, and eventDisplay, a window, and a serial.
instance HasEventType Event where
    eventType = lens X.ev_event_type (\ s v -> s{ X.ev_event_type = v })

instance HasSendEvent Event where
    sendEvent = lens X.ev_send_event (\ s v -> s{ X.ev_send_event = v })

instance HasEventDisplay Event where
    eventDisplay = lens X.ev_event_display (\ s v -> s{ X.ev_event_display = v })

instance HasSerial Event where
    serial = lens X.ev_serial (\ s v -> s{ X.ev_serial = v })

--- Specific events may have an event, a position, dimensions, a border width, and above, a detail, a value mask, an override redirect, a parent, a window, a root, a subwindow, a time, a root position, a state, a keycode, a same screen, a button, a from configure, a request, a first keycode, a count, a mode, a focus, an owner, a requestor, a selection, a target, a property, an atom, a propstate, a message type, a data, a timestamp, a config timestamp, a size indox, a subpixel order, a rotation, an mwidth, an mhight, a subtype, a crtc, an rr mode, an rr width, an rr height, an output, a connection, a property, an rr state, an ss state, an ss kind, or a forced.
instance MayHaveEvent Event where
    _event f = go
        where
        go s@X.ConfigureEvent{} = l s
        go s@X.DestroyWindowEvent{} = l s
        go s@X.UnmapEvent{} = l s
        go s@X.MapNotifyEvent{} = l s
        go s = pure s
        l s = (\ v -> s{ X.ev_event = v }) <$> f (X.ev_event s)

instance MayHavePosition Event where
    _Position f = go
        where
        go s@X.ConfigureRequestEvent{}   = l s
        go s@X.ConfigureEvent{}          = l s
        go s@X.KeyEvent{}                = l s
        go s@X.ButtonEvent{}             = l s
        go s@X.MotionEvent{}             = l s
        go s@X.CrossingEvent{}           = l s
        go s@X.ExposeEvent{}             = l s
        go s@X.RRCrtcChangeNotifyEvent{} = l s
        go s = pure s
        l s = (\p -> s{ X.ev_x = p^.x, X.ev_y = p^.y }) <$> f  ((X.ev_x s, X.ev_y s)^.re position)

instance MayHaveDimensions Event where
    _Dimensions f = go
        where
        go s@X.ConfigureRequestEvent{} = l s
        go s@X.ConfigureEvent{} = l s
        go s@X.ExposeEvent{} = l s
        go s@X.RRScreenChangeNotifyEvent{} = l s
        go s = pure s
        l s = (\d -> s{ X.ev_height = d^.height, X.ev_width = d^.width })
              <$> f ((X.ev_height s, X.ev_width s)^.re dimensions)

instance MayHaveOverrideRedirect Event where
    _overrideRedirect f = go
        where
        go s@X.ConfigureEvent{} = l s
        go s@X.MapNotifyEvent{} = l s
        go s = pure s
        l s = (\ v -> s{ X.ev_override_redirect = v }) <$> f (X.ev_override_redirect s)

instance MayHaveParent Event where
    _parent f = go
       where
       go s@X.ConfigureRequestEvent{} = l s
       go s@X.MapRequestEvent{} = l s
       go s = pure s
       l s = (\ v -> s{ X.ev_parent = v }) <$> f (X.ev_parent s)

instance MayHaveRoot Event where
    _root f = go
       where
       go s@X.KeyEvent{} = l s
       go s@X.ButtonEvent{} = l s
       go s@X.CrossingEvent{} = l s
       go s@X.RRScreenChangeNotifyEvent{} = l s
       -- go s@X.ScreenSaverNotifyEvent{} = l s -- not exported???
       go s = pure s
       l s = (\ w' -> s{ X.ev_root = w' }) <$> f (X.ev_root s)

instance MayHaveRootPosition Event where
    _RootPosition f = go
       where
       go s@X.KeyEvent{} = l s
       go s@X.ButtonEvent{} = l s
       go s@X.CrossingEvent{} = l s
       go s = pure s
       l s = (\r -> s{ X.ev_root = r^.window, X.ev_x_root = r^.x, X.ev_y_root = r^.y }) <$> f ((X.ev_root s, (X.ev_x_root s, X.ev_y_root s)^.re position)^.re rootPosition)

instance MayHaveSubwindow Event where
    _subwindow f = go
       where
       go s@X.CrossingEvent{} = l s
       go s@X.KeyEvent{} = l s
       go s@X.ButtonEvent{} = l s
       go s = pure s
       l s = (\ v -> s{ X.ev_subwindow = v }) <$> f (X.ev_subwindow s)


----- WindowAttributes -----

instance HasPosition WindowAttributes where
    x = lens X.wa_x (\ s v -> s{ X.wa_x = v })
    y = lens X.wa_y (\ s v -> s{ X.wa_y = v })

instance HasDimensions WindowAttributes where
    height = lens X.wa_height (\ s v -> s{ X.wa_height = v })
    width = lens X.wa_width (\ s v -> s{ X.wa_width = v })

instance HasBorderWidth WindowAttributes where
    borderWidth = lens X.wa_border_width (\ s v -> s{ X.wa_border_width = v })

instance HasColormap WindowAttributes where
    colormap = lens X.wa_colormap (\ s v -> s{ X.wa_colormap = v })

instance HasOverrideRedirect WindowAttributes where
    overrideRedirect = lens X.wa_override_redirect (\ s v -> s{ X.wa_override_redirect = v })

mapInstalled :: Lens' WindowAttributes Bool
mapInstalled = lens X.wa_map_installed (\ s v -> s{ X.wa_map_installed =  v })

mapState :: Lens' WindowAttributes CInt
mapState = lens X.wa_map_state (\ s v -> s{ X.wa_map_state = v })


----- WindowChanges -----

instance HasPosition WindowChanges where
    x = lens X.wc_x (\ s v -> s{ X.wc_x = v })
    y = lens X.wc_y (\ s v -> s{ X.wc_y = v })

instance HasDimensions WindowChanges where
    height = lens X.wc_height (\ s v -> s{ X.wc_height = v })
    width = lens X.wc_width (\ s v -> s{ X.wc_width = v })

instance HasBorderWidth WindowChanges where
    borderWidth = lens X.wc_border_width (\ s v -> s{ X.wc_border_width = v })

sibling :: Lens' WindowChanges Window
sibling = lens X.wc_sibling (\ s v -> s{ X.wc_sibling = v })

stackMode :: Lens' WindowChanges CInt
stackMode = lens X.wc_stack_mode (\ s v -> s{ X.wc_stack_mode = v })
