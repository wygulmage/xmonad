
module XMonad.Internal.Optic where

import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Functor.Const (Const (..))
import Lens.Micro.Internal ((#.))

type SimpleOptic f ta a = (a -> f a) -> ta -> f ta

views :: MonadReader s m => SimpleOptic (Const b) s a -> (a -> b) -> m b
-- In general, `views o f` = `f <$> view o f`, but unlike `view`, `views` can `foldMap` a Reader Monad.
views o f = asks (getConst #. o (Const #. f))
