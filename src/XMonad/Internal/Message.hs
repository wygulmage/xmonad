{-# LANGUAGE
    ExistentialQuantification
  #-}

module XMonad.Internal.Message where

import Data.Typeable

class Typeable a => Message a

data SomeMessage = forall a. Message a => SomeMessage a
