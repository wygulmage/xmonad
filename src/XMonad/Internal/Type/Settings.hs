
module XMonad.Internal.Type.Settings where

import Numeric.Natural (Natural)

data Settings = Settings
    { borderWidth :: !Natural
    , focusedBorderColor :: !String
    , normalBorderColor :: !String
    , focusFollowsMouse :: !Bool
    , clickJustFocuses :: !Bool
    }
