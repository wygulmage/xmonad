{-# LANGUAGE
    FlexibleInstances
  #-}

module XMonad.Class.HasName where

import Control.Lens

class HasName a where
    _name :: Lens' a String
