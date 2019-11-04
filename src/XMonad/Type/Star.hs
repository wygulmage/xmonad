{-# LANGUAGE
    NoImplicitPrelude
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , RoleAnnotations
  #-}

module XMonad.Type.Star where

-- All I want is a ReaderT with a Category instance. Profunctor would be nice too, but I'll try not to be greedy and pretend that Arrow is good enough. It's not a MonadTrans, because the parameters are in the 'wrong' order.

import Prelude (fst, snd)
import Control.Applicative (Applicative ((<*>), liftA2, pure), Alternative ((<|>), empty))
import Control.Arrow (Arrow ((&&&), (***), arr, first, second), ArrowChoice ((|||), (+++), left, right), Kleisli (Kleisli, runKleisli))
import Control.Category (Category ((.), id))
import Control.Monad (Monad ((>>=)), (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask, local, reader), ReaderT (ReaderT, runReaderT))

import Data.Functor (Functor ((<$), fmap))
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mempty))
import Data.Coerce (coerce)

import Data.Either (Either (Left, Right), either)

newtype Star m a b = Star{ runStar :: a -> m b }

fromReaderT :: ReaderT a m b -> Star m a b
fromReaderT = coerce

fromKleisli :: Kleisli m a b -> Star m a b
fromKleisli = coerce

toReaderT :: Star m a b -> ReaderT a m b
toReaderT = coerce

toKleisli :: Star m a b -> Kleisli m a b
toKleisli = coerce

-- liftStar = fromReaderT . lift

instance Monad m => Category (Star m) where
    -- (.) :: Star m b c -> Star m a b -> Star m a c
    -- Star g . Star f = Star (g <=< f)
    Star g . Star f = Star (g <=< f)
    -- id :: Star m a a
    id = Star pure

instance Monad m => Arrow (Star m) where
   -- Could reduce to Applicative m constraint; not sure whether that would break laws.
    -- arr :: (a -> b) -> Star m a b
    -- arr f = fmap f id
    arr f = Star (pure . f)
    -- (&&&) :: Star m c a -> Star m c b -> Star m c (a, b)
    (&&&) = liftA2 (,)
    -- (***) :: Star m a b -> Star m c d -> Star m (a, c) (b, d)
    Star f *** Star g = Star (f . fst) &&& Star (g . snd)

instance Monad m => ArrowChoice (Star m) where
    Star f ||| Star g = Star (either f g)
    Star f +++ Star g = Star (fmap Left . f) ||| Star (fmap Right . g)

instance Functor m => Functor (Star m c) where
    -- fmap :: (a -> b) -> Star m c a -> Star m c b
    -- fmap f = fromReaderT . fmap f . toReaderT
    fmap g (Star f) = Star (fmap g . f)
    -- (<$) :: b -> Star m c a -> Star m c b
    x <$ Star f = Star ((x <$) . f)

instance Contravariant m => Contravariant (Star m c) where
    -- contramap :: (b -> a) -> Star m c a -> Star m c b
    contramap f (Star g) = Star (contramap f . g)

instance Applicative m => Applicative (Star m c) where
    -- (<*>) :: Star m c (a -> b) -> Star m c a -> Star m c b
    liftA2 f (Star gx) (Star hx) = Star (\ x -> liftA2 f (gx x) (hx x))
    -- pure :: a -> Star m c a
    -- pure = (<$ id)
    pure = Star . pure . pure

instance Alternative m => Alternative (Star m c) where
    -- (<|>) :: Star m c a -> Star m c a -> Star m c a
    Star f <|> Star g = Star (\ x -> f x <|> g x)
    -- empty :: Star m c a
    empty = Star (pure empty)

instance Monad m => Monad (Star m c) where
    -- (>>=) :: Star m c a -> a -> Star m c b -> Star m c b
    mx >>= f = Star
        (\ x -> do
            y <- runStar mx x
            runStar (f y) x)

instance Semigroup (m b) => Semigroup (Star m a b) where
    -- (<>) :: Star m a b -> Star m a b -> Star m a b
    Star f <> Star g = Star (\ x -> f x <> f x)

instance Monoid (m b) => Monoid (Star m a b) where
    -- mempty :: Star m a b
    mempty = Star (pure mempty)

instance Monad m => MonadReader c (Star m c) where
    ask = fromReaderT ask
    local f = fromReaderT . local f . toReaderT
    reader f = fromReaderT (reader f)

instance MonadIO m => MonadIO (Star m c) where
    liftIO = fromReaderT . liftIO
