{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-} -- for MonadError

module XMonad.Internal.Type.Star
    (Star (Star, runStar))
  where

import Prelude (Either (Left, Right), either, flip, fst, snd, uncurry)
import Data.Coerce (coerce)

import Control.Category (Category (id, (.)))
import Control.Arrow
    ( Arrow (arr, (&&&), (***))
    , ArrowChoice ((+++), (|||))
    , ArrowApply (app)
    , ArrowZero (zeroArrow)
    , ArrowPlus ((<+>))
    , ArrowLoop (loop)
    )

import Data.Functor (Functor ((<$), fmap))
import Data.Functor.Contravariant (Contravariant (contramap))
import Control.Applicative
    ( Applicative ((<*>), (*>), liftA2, pure)
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
import Data.Semigroup (Semigroup ((<>)))


------- Types -------

newtype Star m a b = Star{ runStar :: a -> m b }
-- equivalent to:
--   `Kleisli m a b`
--   `Compose ((->) a) m b`
--   (permuted) `ReaderT a m b`


------- Helper Functions -------

constStar :: m a -> Star m c a
-- ^ Construct a Star from a value in a context.
constStar = Star . pure -- Use `(->) c`'s `pure`.
{-# INLINE constStar #-}

lmapStar :: (b -> a) -> Star m a c -> Star m b c
-- lmapStar f = lowerStar (. f) -- Coerce `->`'s `lmap`.
lmapStar = lmapWith (flip (.))
{-# INLINE lmapStar #-}

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

productWith ::
    (m1 a -> m2 b -> m3 d) ->
    Star m1 c a -> Star m2 c b -> Star m3 c d
productWith = lowerStar2 . liftA2 -- Coerce `(->) c`'s `liftA2`.
{-# INLINE productWith #-}

bindWith ::
    ((a -> m1 b1) -> m2 b2 -> m3 b3) ->
    (a -> Star m1 c b1) -> Star m2 c b2 -> Star m3 c b3
bindWith f = lowerStar . liftA2 f . flipRunStar
{-# INLINE bindWith #-}
-- ^ Take a 'bind' function for 'm's and return a 'bind' function for 'Star m c's. Uses below are flipped because Haskell normally flips bind functions.
-- The `flip` gets you `c -> a -> m b` from `a -> Star m c b`.

flipRunStar ::
    (a -> Star m c b) ->
    c -> a -> m b
flipRunStar = flip . (runStar .)
{-# INLINE flipRunStar #-}

mapWith ::
    (a -> m b -> n d) ->
    a -> Star m c b -> Star n c d
mapWith f = lowerStar . fmap . f
{-# INLINE mapWith #-}

lmapWith ::
    (a -> (b -> m c) -> d -> n e) ->
    a -> Star m b c -> Star n d e
lmapWith = fmap lowerStar
{-# INLINE lmapWith #-}

composeWith ::
    Monad m =>
    ((m a -> m b) -> (a2 -> m2 b2) -> a3 -> m3 b3) ->
    Star m a b -> Star m2 a2 b2 -> Star m3 a3 b3
composeWith f = lowerStar2 (f . (=<<))
{-# INLINE composeWith #-}

-- type class homomorphism: method (hom x) = hom (method x)
-- For example, fmap f (hom x) = hom (fmap f x)

------- Category Hierarchy Instances -------

instance Monad m => Category (Star m) where
    -- (.) = lowerStar2 (<=<)
    (.) = composeWith (.)
    id = Star pure

instance Monad m => Arrow (Star m) where
    (&&&) = liftA2 (,)
    f *** g = lmapStar fst f &&& lmapStar snd g
    arr f = lmapStar f id

instance Monad m => ArrowChoice (Star m) where
    (|||) = lowerStar2 either
    f +++ g = fmap Left f ||| fmap Right g

instance Monad m => ArrowApply (Star m) where
    app = Star (uncurry runStar)

instance (Alternative m, Monad m) => ArrowZero (Star m) where
    zeroArrow = empty

instance (Alternative m, Monad m) => ArrowPlus (Star m) where
    (<+>) = (<|>)

instance MonadFix m => ArrowLoop (Star m) where
    loop (Star f) = Star (fmap fst . mfix . f')
      where
        f' x y = f (x, snd y)

------- Functor Hierarchy Instances -------

instance Functor m => Functor (Star m c) where
    fmap = mapWith fmap
    (<$) = mapWith (<$)

instance Contravariant m => Contravariant (Star m c) where
    contramap f = lowerStar (contramap f .)

instance Applicative m => Applicative (Star m c) where
    (<*>) = productWith (<*>)
    (*>) = productWith (*>)
    liftA2 = productWith . liftA2
    pure = constStar . pure

instance Alternative m => Alternative (Star m c) where
    (<|>) = productWith (<|>)
    empty = constStar empty

instance Monad m => Monad (Star m c) where
    (>>=) = flip (bindWith (=<<))
    (>>) = (*>)

------- Monad Utility Instances -------

instance MonadFail m => MonadFail (Star m c) where
    fail = constStar . fail

instance MonadFix m => MonadFix (Star m c) where
    mfix = Star . fmap mfix . flipRunStar

instance MonadIO m => MonadIO (Star m c) where
    liftIO = constStar . liftIO

instance MonadZip m => MonadZip (Star m c) where
    mzipWith = productWith . mzipWith


------- MTL Instances -------

instance Monad m => MonadReader c (Star m c) where
    ask = id
    local = lmapStar
    reader f = lmapStar f id

instance MonadCont m => MonadCont (Star m c) where
    callCC f =
        Star (\ x -> callCC (flipRunStar f x . fmap constStar))

instance MonadError e m => MonadError e (Star m c) where
    catchError = flip (bindWith (flip catchError))
    throwError = constStar . throwError

instance MonadState s m => MonadState s (Star m c) where
    get = constStar get
    put = constStar . put
    state = constStar . state

instance MonadWriter w m => MonadWriter w (Star m c) where
    listen = lowerStar (listen .)
    pass = lowerStar (pass .)
    tell = constStar . tell
    writer = constStar . writer


------- Semigroup Hierarchy Instances -------

instance Semigroup (m b) => Semigroup (Star m a b) where
    (<>) = productWith (<>)

instance Monoid (m b) => Monoid (Star m a b) where
    mempty = constStar mempty


------- Cruft Instances -------

instance (Alternative m, Monad m) => MonadPlus (Star m c) where
    mplus = (<|>)
    mzero = empty
