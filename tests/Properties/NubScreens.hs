
module Properties.NubScreens (
  prop_nubScreens_equal,
  )where

import Test.QuickCheck
import Instances
import Utils

import XMonad.Operations (nubScreens)

prop_nubScreens_equal x = nubScreens (x : x : []) == (x : [])
