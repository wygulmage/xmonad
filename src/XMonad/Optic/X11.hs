module XMonad.Optic.X11 where

import Control.Lens
import Foreign.C.Types (CInt, CULong)
import Graphics.X11.Types (Colormap, EventType, Window)
import Graphics.X11.Xlib (Display)
import qualified Graphics.X11.Xlib as X
import Graphics.X11.Xlib.Extras (Event, WindowAttributes (WindowAttributes), WindowChanges (WindowChanges))
import qualified Graphics.X11.Xlib.Extras as X


----- Lens Classes -----
--- NOTE: 'x' and 'y' are in scope unqualified as methods. Don't use them as variables!

class HasX t where
  x :: Lens' t CInt

class HasY t where
  y :: Lens' t CInt

class HasHeight t where
  height :: Lens' t CInt

class HasWidth t where
  width :: Lens' t CInt

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


----- Affine Traversal Classes -----

class MayHaveX t where
  _x :: Traversal' t CInt

class MayHaveY t where
  _y :: Traversal' t CInt

class MayHaveHeight t where
    _height :: Traversal' t CInt

class MayHaveWidth t where
    _width :: Traversal' t CInt

class MayHaveBorderWidth t where
    _borderWidth :: Traversal' t CInt

class MayHaveEvent t where
    _event :: Traversal' t Window

class MayHaveParent t where
    _parent :: Traversal' t Window

----- Event -----

instance MayHaveEvent Event where
    _event f = go
        where
        go s@X.ConfigureEvent{}     = (\ v -> s{ X.ev_event = v }) <$> f (X.ev_event s)
        go s@X.DestroyWindowEvent{} = (\ v -> s{ X.ev_event = v }) <$> f (X.ev_event s)
        go s@X.UnmapEvent{}         = (\ v -> s{ X.ev_event = v }) <$> f (X.ev_event s)
        go s@X.MapNotifyEvent{}     = (\ v -> s{ X.ev_event = v }) <$> f (X.ev_event s)
        go s = pure s

instance MayHaveX Event where
    _x f = go
        where
        go s@X.ConfigureRequestEvent{} = (\ v -> s{ X.ev_x = v }) <$> f (X.ev_x s)
        go s@X.ConfigureEvent{} = (\ v -> s{ X.ev_x = v }) <$> f (X.ev_x s)
        go s@X.KeyEvent{} = (\ v -> s{ X.ev_x = v }) <$> f (X.ev_x s)
        go s@X.KeyEvent{} = (\ v -> s{ X.ev_x = v }) <$> f (X.ev_x s)
        go s@X.ButtonEvent{} = (\ v -> s{ X.ev_x = v }) <$> f (X.ev_x s)
        go s@X.MotionEvent{} = (\ v -> s{ X.ev_x = v }) <$> f (X.ev_x s)
        go s@X.CrossingEvent{} = (\ v -> s{ X.ev_x = v }) <$> f (X.ev_x s)


instance HasEventType Event where
    eventType = lens X.ev_event_type (\ s v -> s{ X.ev_event_type = v })

instance HasSendEvent Event where
    sendEvent = lens X.ev_send_event (\ s v -> s{ X.ev_send_event = v })



----- WindowAttributes -----

instance HasX WindowAttributes where
    x = lens X.wa_x (\ s v -> s{ X.wa_x = v })

instance HasY WindowAttributes where
    y = lens X.wa_y (\ s v -> s{ X.wa_y = v })

instance HasHeight WindowAttributes where
    height = lens X.wa_height (\ s v -> s{ X.wa_height = v })

instance HasWidth WindowAttributes where
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
