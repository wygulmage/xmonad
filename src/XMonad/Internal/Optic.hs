
module XMonad.Internal.Optic where

import Control.Applicative (Alternative (empty))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Functor.Const (Const (..))
import Data.Semigroup (Endo)
import Lens.Micro (Getting)
import Lens.Micro.Internal ((#.), foldrOf)

type SimpleOptic f ta a = (a -> f a) -> ta -> f ta

views :: MonadReader s m => SimpleOptic (Const b) s a -> (a -> b) -> m b
-- In general, `views o f` = `f <$> view o f`, but unlike `view`, `views` can `foldMap` a Reader Monad.
views o f = asks (getConst #. o (Const #. f))


--- Is it better to provide Traversal variants of Foldable methods, or to use the Foldable methods with toListOf? Lists are a bit more versatile, since you can `filter p . toListOf` but there's no 'filterOf'.

findOf :: Alternative m => Getting (Endo (m a)) ta a -> (a -> Bool) -> ta -> m a
findOf o p = foldrOf o (\ x xs -> if p x then pure x else xs) empty
-- findOf o p = find p . toListOf
