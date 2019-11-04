
module XMonad.Type.Star where

-- All I want is a ReaderT with a Category instance. Profunctor would be nice too, but I'll try not to be greedy and pretend that Arrow is good enough. It's not a MonadTrans, because the parameters are in the 'wrong' order.

import Control.Applicative (Applicative ((<*>), pure), Alternative ((<|>), empty))
import Control.Arrow (Arrow ((***), arr, first, second))
import Control.Category (Category ((.), id), Kleisli (..))
import Control.Monad (Monad ((>>=)), (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask, local, reader), ReaderT (..))

import Data.Functor (Functor (fmap))
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Semigroup (Semigroup (<>))
import Data.Monoid (Monoid (mempty))
import Data.Coerce (coerce)

newtype Star m a b = Star{ runStar :: a -> m b }

fromReaderT :: ReaderT a m b -> Star m a b
fromReaderT = coerce

fromKleisli :: Kleisli m a b -> Star m a b
fromKleisli = coerce

toReaderT :: Star m a b -> ReaderT a m b
toReaderT = coerce

toKleisli :: Star m a b -> Kleisli m a b
toKleisli = coerce

instance Monad m => Category (Star m) where
    g . f = fromKleisli (toKleisli g . toKleisli f)
    id = fromKleisli id

instance Applicative m => Arrow (Star m) where
    arr f = fromKleisli (arr f)
    first f = fromKleisli . first f . toKleisli
    second f = fromKleisli . second f . toKleisli
    f *** g = fromKleisli (toKleisli f *** toKleisli g)

instance Functor m => Functor (Star m c) where
    fmap f = fromReaderT . fmap f . toReaderT
    (<$) x = fromReaderT . (x <$) . toReaderT

instance Contravariant m => Contravariant (Star m c) where
    contramap f (Star g) = Star (contramap f . g)

instance Applicative m => Applicative (Star m c) where
    mf <*> mx = fromReaderT (toReaderT mf <*> toReaderT mx)
    pure = fromReaderT pure

instance Alternative m => Alternative (Star m c) where
    f <|> g = fromReaderT (toReaderT f <|> toReaderT g)
    empty = fromReaderT empty

instance Monad m => Monad (Star m c) where
    mx >>= f = fromReaderT (toReaderT mx >>= f))

instance Semigroup (m b) => Semigroup (Star m a b) where
    Star f <> Star g = Star (\ x -> f x <> f g)

instance Monoid (m b) => Monoid (Star m a b) where
    mempty = Star (pure mempty)

instance Monad m => MonadReader c (Star m c) where
    ask = fromReaderT ask
    local f = fromReaderT . local f . toReaderT
    reader f = fromReaderT (asks f)
