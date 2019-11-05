{-# LANGUAGE
    NoImplicitPrelude
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module XMonad.Internal.Type.Star where


import Prelude (flip, fst, snd)

import Control.Applicative (Applicative (liftA2, pure), Alternative ((<|>), empty))
import Control.Arrow (Arrow ((&&&), (***), arr), ArrowChoice ((|||), (+++)))
import Control.Category (Category ((.), id))
import Control.Monad (Monad ((>>=)), (=<<), (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask, local, reader))

import Data.Coerce (coerce)
import Data.Functor (Functor (fmap))
import Data.Either (Either (Left, Right), either)
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mempty))


newtype Star m a b = Star{ runStar :: a -> m b }

lmapStar :: (b -> a) -> Star m a c -> Star m b c
lmapStar f = unStarred (. f)
{-# INLINE lmapStar #-}

unStarred :: ((a -> m b) -> c -> n d) -> Star m a b -> Star n c d
unStarred f = Star . f . runStar
{-# INLINE unStarred #-}

productWith :: (m b -> n c -> o d) -> Star m a b -> Star n a c -> Star o a d
productWith f (Star g) (Star h) = Star (\ x -> f (g x) (h x))
{-# INLINE productWith #-}


instance Monad m => Category (Star m) where
    Star g . Star f = Star (g <=< f)
    id = Star pure

instance Monad m => Arrow (Star m) where
    (&&&) = liftA2 (,)
    f *** g = lmapStar fst f &&& lmapStar snd g
    arr f = Star (pure . f)

instance Monad m => ArrowChoice (Star m) where
    Star f ||| Star g = Star (either f g)
    f +++ g = fmap Left f ||| fmap Right g


instance Functor m => Functor (Star m c) where
    fmap g = unStarred (fmap g .)

instance Applicative m => Applicative (Star m c) where
    liftA2 f = productWith (liftA2 f)
    pure = Star . pure . pure

instance Alternative m => Alternative (Star m c) where
    (<|>) = productWith (<|>)
    empty = Star (pure empty)

instance Monad m => Monad (Star m c) where
    Star f >>= g = Star (\ x -> flip (runStar . g) x =<< f x)

instance MonadIO m => MonadIO (Star m c) where
    liftIO = arr liftIO

instance Monad m => MonadReader c (Star m c) where
    ask = id
    local = lmapStar
    reader = arr


instance Semigroup (m b) => Semigroup (Star m a b) where
    (<>) = productWith (<>)

instance Monoid (m b) => Monoid (Star m a b) where
    mempty = Star (pure mempty)
