{-# LANGUAGE GADTs, RankNTypes #-}

module XMonad.Message where

import Data.Typeable (Typeable)

-- | Based on ideas in /An Extensible Dynamically-Typed Hierarchy of
-- Exceptions/, Simon Marlow, 2006. Use extensible messages to the
-- 'handleMessage' handler.
--
-- User-extensible messages must be a member of this class.
class Typeable a => Message a

data SomeMessage = forall a. Message a => SomeMessage a
