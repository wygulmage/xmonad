
module XMonad.EwmhDesktops (
    ) where

import Codec.Binary.UTF8.String

import qualified Data.Bits as Bits
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import XMonad (ExtensionClass (initialValue), Window, none)
import qualified XMonad.ExtensibleState as ES


data EwmhState = EwmhState
    ![String]         -- DesktopNames
    ![Window]         -- ClientList
    !Int              -- CurrentDesktop
    !(Map Window Int) -- WindowDesktops
    !Window           -- ActiveWindow
    !(Maybe Window)   -- NetActivated

instance ExtensionClass EwmhState where
    initialValue =
        EwmhState [] [] (-1) Map.empty (Bits.complement none) Nothing
