{-# LANGUAGE
     NoImplicitPrelude
   , ConstraintKinds
   , FlexibleContexts
   , FlexibleInstances
   , InstanceSigs
   , MultiParamTypeClasses
   , QuantifiedConstraints
   , RankNTypes
   , TypeInType
   , UndecidableInstances
   #-}

module XMonad.Optic.Functor where


import Control.Category (Category ((.), id))
import Data.Functor (Functor (fmap))
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Traversable (Traversable) -- TODO: Create Traversable that depends of Inpair?
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mempty))
import Data.Either (Either (Left, Right), either)
import Data.Functor.Const (Const (Const), getConst)


class Inpair m where
    -- ^ 'Inpair' is equivalent to 'Apply' (the '(<*>)' half of Applicative), but admits contravariant functors.
    -- An instance of Inpair MUST be an instance of Functor or Contravariant.
    inpair :: m a -> m b -> m (a, b)


class Inpair m => Point m where
    -- ^ The 'pure' half of Applicative and Monad, for Inpair. Note that to define 'point' for a Contravariant m, it may be easiest to factor through unitPoint.
    point :: a -> m a

unitPoint :: Point m => m ()
unitPoint = point ()


class (Functor m, Inpair m) => Bind m where
    -- ^ Bind is the '(=<<)' half of Monad, but requiring Impair instead of Apply.
    bind :: (a -> m b) -> m a -> m b
    bind f = join . fmap f
    join :: m (m a) -> m a
    join = bind id

class Functor m => Extend m where
    -- ^ Extend is the '(<<=)' half of Comonad. Should really be taking this from the Semigroupoids package.
    -- Laws:
    --    extend g . extend f = extend (g . extend f)
    --    duplicate . duplicate = fmap duplicate . duplicate
    --    extend f = fmap f . duplicate
    --    duplicate = extend id
    extend :: (m a -> b) -> m a -> m b
    extend f = fmap f . duplicate
    duplicate :: m a -> m (m a)
    duplicate = extend id

class Extend m => Extract m where
    -- ^ Extract is the 'extract' half of Comonad. Should really be taking this from the Semigroupoids package.
    -- Laws:
    --    extend extract = id
    --    extract . extend f = f
    --    extract . duplicate = id
    --    fmap extract . duplicate = id
    extract :: m a -> a


instance Inpair ((->) a) where
    inpair :: (a -> b) -> (a -> c) -> a -> (b, c)
    inpair f g x = (f x, g x)

instance Point ((->) c) where
    point x _ = x

instance Bind ((->) a) where
    join :: (a -> a -> b) -> a -> b
    join f x = f x x

instance Semigroup c => Extend ((->) c) where
    duplicate :: (c -> a) -> c -> c -> a
    duplicate f x y = f (x <> y)

instance Monoid c => Extract ((->) c) where
    extract :: (c -> a) -> a
    extract f = f mempty


instance Semigroup c => Inpair ((,) c) where
    inpair :: (c, a) -> (c, b) -> (c, (a, b))
    inpair ~(u, x) ~(v, y) = (u <> v, (x, y))

instance Semigroup c => Bind ((,) c) where
    bind :: (a -> (c, b)) -> (c, a) -> (c, b)
    bind f ~(u, x) = case f x of ~(v, y) -> (u <> v, y)

instance Monoid c => Point ((,) c) where
    point = (,) mempty

instance Extend ((,) c) where
    extend f ~ux@(u, _) = (u, f ux)

instance Extract ((,) c) where
    extract (_, x) = x


instance Inpair (Either c) where
    inpair :: Either c a -> Either c b -> Either c (a, b)
    inpair (Left x) = point (Left x)
    inpair (Right x) = either Left (Right . (,) x)

instance Point (Either c) where
    point = Right

instance Bind (Either c) where
    bind :: (a -> Either c b) -> Either c a -> Either c b
    bind f = either Left f

instance Extend (Either c) where
    extend :: (Either c a -> b) -> Either c a -> Either c b
    extend f = Right . f
    -- There are two ways to do this.
    -- When duplicate always wraps in Right, it always allows the 'extend' chain to continue.
    -- extend g . extend f = extend (g . extend f)
    --     (Right . g) . (Right . f)
    --     = Right . (g . Right . f)
    -- When it only wraps Right values, it ends the 'extend' chain at the first Left. Which is strange, because extend takes a function that accepts left or right.
    --     either Left (Right . f . Right) . either Left (Right . g . Right)
    --     = either Left (Right . f . Right . g . Right)
    --     = either Left (Right . (f . either Left (Right . g . Right)))
    --     = extend (g . extend f)
    --Ed Kmett uses the second version. Honestly, between the choice of never applying the function to a Left value and always re-wrapping things in Right, I'm not convinced that there should be an Extend instance for Either. It almost seems like an argument against having the class at all, except that would force a Monoid constraint on totally reasonable Semigroup-based extends.



------- Bifunctors -------

class (forall c. Functor (p c)) => Bifunctor p where
    {-# MINIMAL first | bimap #-}
    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap g f = first g . fmap f

class (forall c. Functor (p c)) => Profunctor p where
    {-# MINIMAL lmap | dimap #-}
    lmap :: (b -> a) -> p a c -> p b c
    lmap f = dimap f id
    dimap :: (b -> a) -> (c -> d) -> p a c -> p b d
    dimap g f = lmap g . fmap f

class Profunctor p => Endomap m p where -- Could allow all bifunctors.
    endomap :: p a b -> p (m a) (m b)

class (forall c. Endomap ((,) c) p) => Prostrong p

second' :: Prostrong p => p a b -> p (c, a) (c, b)
second' = endomap

class (forall c. Endomap (Either c) p) => Prochoice p

right' :: Prochoice p => p a b -> p (Either c a) (Either c b)
right' = endomap

class (Prochoice p, Prostrong p) => Traversing p

traverse' :: (Traversing p, Inpair m, Endomap m p) => p a b -> p (m a) (m b)
traverse' = endomap

class (forall c. Endomap ((->) c) p) => Closed p

closed :: Closed p => p a b -> p (c -> a) (c -> b)
closed = endomap

curry :: Closed p => p (a, b) c -> p a (b -> c)
curry = lmap (,) . closed

-- class (Traversing p, Closed p) => Mapping p
class (forall a. Functor a => Endomap a p) => Mapping p
instance (forall a. Functor a => Endomap a p) => Mapping p

map :: Mapping p  => Functor m => p a b -> p (m a) (m b)
map = endomap


newtype Flip f a b = Flip { unFlip :: f b a }

instance Bifunctor p => Bifunctor (Flip p) where
    first f = Flip . fmap f . unFlip
    bimap f g = Flip . bimap g f . unFlip

instance Bifunctor p => Functor (Flip p c) where
    fmap f = Flip . first f . unFlip

instance Profunctor p => Contravariant (Flip p c) where
    contramap f = Flip . lmap f . unFlip


instance Profunctor (->) where
    lmap f = (. f)

instance Functor m => Endomap m (->) where
    endomap = fmap

instance Prochoice (->)
instance Prostrong (->)
instance Traversing (->)
instance Closed (->)
instance Mapping (->)


instance Bifunctor (,) where
    bimap g f ~(x, y) = (g x, f y)


instance Bifunctor Either where
    bimap g f = either (Left . g) (Right . f)


instance Bifunctor Const where
    first f = Const . f . getConst

instance Profunctor (Flip Const) where
    lmap _ = Flip. Const . getConst . unFlip
