{-# LANGUAGE FlexibleContexts
           , PatternGuards
           , ScopedTypeVariables
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ExtensibleState
-- Copyright   :  (c) Daniel Schoepe 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  daniel.schoepe@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Module for storing custom mutable state in xmonad.
--
-----------------------------------------------------------------------------

module XMonad.ExtensibleState (
  put,
  modify,
  modified,
  remove,
  get,
  gets,
  ) where

import           Control.Monad.State (MonadState, State)
import qualified Control.Monad.State as State

import Data.Functor (($>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Proxy (Proxy), cast, typeOf, typeRep)

import Text.Read (readMaybe)

import XMonad
    (ExtensionClass, StateExtension (StateExtension, PersistentExtension), XState, extensibleState, extensionType, initialValue, stateExtensions)

-- | Modify the Map of state extensions.
modifyStateExts ::
    (MonadState XState m)=>
    (Map String (Either String StateExtension) ->
        Map String (Either String StateExtension)) ->
    m ()
modifyStateExts f =
   State.modify $ \ st -> st{ extensibleState = f (extensibleState st) }
{-# INLINE modifyStateExts #-}

modify :: (ExtensionClass a, MonadState XState m)=> (a -> a) -> m ()
modify f = get >>= put . f
{-# INLINE modify #-}

-- | Save the state of an extension.
put :: (ExtensionClass a, MonadState XState m)=> a -> m ()
put x =
    modifyStateExts . Map.insert (show $ typeOf x) . Right $ extensionType x
{-# INLINE put #-}

-- | Revert a extension's state to its initial value.
remove :: forall m a. (ExtensionClass a, MonadState XState m)=> a -> m ()
-- remove _ = modifyStateExts $ Map.delete (show $ typeOf (initialValue :: a))
remove _ = modifyStateExts $ Map.delete (show $ typeRep (Proxy :: Proxy a))
{-# INLINE remove #-}

-- | Return the current state of an extension.
get :: forall m a. (ExtensionClass a, MonadState XState m)=> m a
get = State.state $ State.runState get'
{-# INLINE get #-}

-- | Return the result of a function applied to the current state of an extension.
gets :: (ExtensionClass a, MonadState XState m)=> (a -> b) -> m b
gets = (`fmap` get)
{-# INLINE gets #-}

-- | Determine whether a function altered the state; if it did, save the new state and return @True@, otherwise return @False@.
modified :: (ExtensionClass a, Eq a, MonadState XState m)=> (a -> a) -> m Bool
modified f = do
    x <- get
    case f x of
        x'
            | x == x'   -> pure False
            | otherwise -> put x' $> True
{-# INLINE modified #-}

get' :: forall a. (ExtensionClass a)=> State XState a
get' = do
    ex <- State.gets $ Map.lookup (show $ typeOf default_) . extensibleState
    case ex of
       Just (Right (StateExtension x))      | Just x' <- cast x -> pure x'
       Just (Right (PersistentExtension x)) | Just x' <- cast x -> pure x'
       Just (Left cs)
         | PersistentExtension px <- extensionType default_
         , Just x' <- cast =<< readMaybe cs `asTypeOf` Just px ->
             put x' $> x'
       _ -> pure default_
  where
    default_ :: a
    default_ = initialValue

{- Note on get' and INLINE
forall m. MonadState XState m is equivalent to State XState. The former, polymorphic version is much more useful because it can be instantiate as X. The latter is much more efficient because the monadic bind is known statically and can be inlined.

So small functions are kept polymorphic and inlined, while large functions are instantiated with a concrete State Monad and then lifted to an inlined Monad-polymorphic version.
-}
