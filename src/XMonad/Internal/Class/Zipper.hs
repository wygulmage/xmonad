{-# LANGUAGE
    TypeFamilies
  #-}

module XMonad.Internal.Class.Zipper where

import Lens.Micro

class Zipper ma where
  type Item ma
  _focus :: Lens' ma (Item ma)
  _up :: Lens' ma [Item ma]
  _dn :: Lens' ma [Item ma]
