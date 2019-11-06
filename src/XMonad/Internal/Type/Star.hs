{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-} -- for MTL classes

module XMonad.Internal.Type.Star
    ( Star (Star, runStar)
    , lmapStar, firstStar, secondStar
    )
  where

import Prelude (Either (Left, Right), either, flip, fst, snd, uncurry)
import Data.Coerce (coerce)

import Control.Category (Category (id, (.)))
import Control.Arrow
    ( Arrow (arr, (&&&), (***), first, second)
    , ArrowChoice ((+++), (|||))
    , ArrowApply (app)
    , ArrowZero (zeroArrow)
    , ArrowPlus ((<+>))
    , ArrowLoop (loop)
    )

import Data.Functor (Functor ((<$), fmap))
import Data.Functor.Contravariant (Contravariant (contramap))
import Control.Applicative
    ( Applicative ((<*>), (<*), (*>), liftA2, pure)
    , Alternative (empty, (<|>))
    )
import Control.Monad
    ( Monad ((>>=), (>>)), (<=<), (=<<)
    , MonadPlus (mplus, mzero)
    )

import Control.Monad.Fail (MonadFail (fail))
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Zip (MonadZip (mzipWith))

import Control.Monad.Reader (MonadReader (ask, local, reader))
import Control.Monad.Cont (MonadCont (callCC))
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.State (MonadState (get, put, state))
import Control.Monad.Writer (MonadWriter (listen, pass, tell, writer))

import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>), stimes))


------- Types -------

newtype Star m a b = Star{ runStar :: a -> m b }
type role Star representational representational nominal
-- Mostly equivalent to:
--   `Kleisli m a b`
--   `Compose ((->) a) m b`
--   (permuted) `ReaderT a m b`


------- Helper Functions -------

constStar :: m a -> Star m c a
constStar = Star . pure -- Use `(->) c`'s `pure`.
{-# INLINE constStar #-}

lowerStar ::
    ((a -> m b) -> c -> n d) ->
    Star m a b -> Star n c d
lowerStar = coerce
{-# INLINE lowerStar #-}

lowerStar2 ::
     ((a1 -> m1 b1) -> (a2 -> m2 b2) -> a3 -> m3 b3) ->
     Star m1 a1 b1 -> Star m2 a2 b2 -> Star m3 a3 b3
lowerStar2 = coerce
{-# INLINE lowerStar2 #-}

bindWith ::
    ((a -> m1 b1) -> m2 b2 -> m3 b3) ->
    (a -> Star m1 c b1) -> Star m2 c b2 -> Star m3 c b3
bindWith f = lowerStar . liftA2 f . flipRunStar
{-# INLINE bindWith #-}
-- ^ Lift a 'bind' function for 'm's into a function to 'Star m c's.
-- The `flipRunStar` gets you `c -> a -> m b` from `a -> Star m c b`.

flipRunStar ::
    (a -> Star m c b) ->
    c -> a -> m b
flipRunStar = flip . coerce
{-# INLINE flipRunStar #-}


------- Profunctor Methods -------

lmapStar :: (b -> a) -> Star m a c -> Star m b c
lmapStar f = lowerStar (. f) -- Use `->`'s `lmap`.
{-# INLINE lmapStar #-}

firstStar :: Functor m => Star m a b -> Star m (a, c) (b, c)
firstStar f = Star (\ ~(x, y) -> fmap (\ x' -> (x', y)) (runStar f x))

secondStar :: Functor m => Star m a b -> Star m (c, a) (c, b)
secondStar f = Star (\ ~(x, y) -> fmap ((,) x) (runStar f y))


------- Category Hierarchy Instances -------

instance Monad m => Category (Star m) where
    (.) = lowerStar2 (<=<)
    id = Star pure

instance Monad m => Arrow (Star m) where
    (&&&) = liftA2 (,)
    f *** g = lmapStar fst f &&& lmapStar snd g
    arr f = lmapStar f id -- or `Star (pure . f)`
    first = firstStar
    second = secondStar

instance Monad m => ArrowChoice (Star m) where
    (|||) = lowerStar2 either
    f +++ g = fmap Left f ||| fmap Right g

instance Monad m => ArrowApply (Star m) where
    -- app :: Star m (Star m a b, a) b
    app = Star (uncurry runStar)

instance (Alternative m, Monad m) => ArrowZero (Star m) where
    zeroArrow = empty

instance (Alternative m, Monad m) => ArrowPlus (Star m) where
    (<+>) = (<|>)

instance MonadFix m => ArrowLoop (Star m) where
    -- loop :: Star m (a, c) (b, c) -> Star m a b
    loop (Star f) = Star (fmap fst . mfix . f')
      where
        f' x y = f (x, snd y)

------- Functor Hierarchy Instances -------

instance Functor m => Functor (Star m c) where
    fmap f = lowerStar (fmap (fmap f))
    (<$) x = lowerStar (fmap (x <$))

instance Contravariant m => Contravariant (Star m c) where
    contramap f = lowerStar (contramap f .)

instance Applicative m => Applicative (Star m c) where
    (<*>) = lowerStar2 (liftA2 (<*>))
    (<*)  = lowerStar2 (liftA2 (<*))
    (*>)  = lowerStar2 (liftA2 (*>))
    liftA2 = lowerStar2 . liftA2 . liftA2
    pure = Star . pure . pure

instance Alternative m => Alternative (Star m c) where
    -- Use `m`'s Alternative instance.
    (<|>) = lowerStar2 (liftA2 (<|>))
    empty = Star (pure empty)

instance Monad m => Monad (Star m c) where
    -- Thread `m`'s (=<<) through `->`.
    (>>=) = flip (bindWith (=<<))
    (>>) = (*>)

------- Monad Utility Instances -------

instance MonadFail m => MonadFail (Star m c) where
    -- Use `m`'s `fail`.
    fail = Star . pure . fail

instance MonadFix m => MonadFix (Star m c) where
    -- mfix :: (a -> Star m c a) -> Star m c a
    mfix = Star . fmap mfix . flipRunStar

instance MonadIO m => MonadIO (Star m c) where
    -- Use `m`'s `liftIO`.
    -- liftIO :: IO a -> Star m c a
    liftIO = Star . pure . liftIO

instance MonadZip m => MonadZip (Star m c) where
    -- Use `m`'s `mzipWith`.
    -- mzipWith :: (a -> b -> d) -> Star m c a -> Star m c b -> Star m c d
    mzipWith = lowerStar2 . liftA2 . mzipWith


------- MTL Instances -------

instance Monad m => MonadReader c (Star m c) where
    ask = id
    local = lmapStar
    reader f = lmapStar f id

instance MonadCont m => MonadCont (Star m c) where
    callCC f =
        Star (\ x -> callCC (flipRunStar f x . fmap constStar))

instance MonadError e m => MonadError e (Star m c) where
    -- Bind `m`'s `catchError` through `->`.
    catchError = flip (bindWith (flip catchError))
    -- Use `m`'s `throwError`.
    throwError = Star . pure . throwError

instance MonadState s m => MonadState s (Star m c) where
    -- Use `m`'s methods.
    get = Star (pure get)
    put = Star . pure . put
    state = Star . pure . state

instance MonadWriter w m => MonadWriter w (Star m c) where
    listen = lowerStar (listen .)
    pass = lowerStar (pass .)
    tell = constStar . tell
    writer = constStar . writer


------- Semigroup Hierarchy Instances -------

instance Semigroup (m b) => Semigroup (Star m a b) where
    -- Use `->`'s Semigroup instance (which uses `m b`'s).
    (<>) = lowerStar2 (<>)
    stimes = lowerStar . stimes

instance Monoid (m b) => Monoid (Star m a b) where
    -- `->`'s Monoid instance uses that of its result.
    mempty = Star mempty


------- Cruft Instances -------

instance (Alternative m, Monad m) => MonadPlus (Star m c) where
    mplus = (<|>)
    mzero = empty
