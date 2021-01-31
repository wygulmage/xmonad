{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XMonad.Internal.Optics (
    (%~), (.~),
    (%%~),
    traverseOf_, forOf_,
    sets,
    (^.), (^?), (^..),
    to,
    (&), (<&>), -- re-exported
    ) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Functor.Contravariant (Contravariant ((>$), contramap))
import Data.Semigroup (Endo (Endo, appEndo))
import Data.Monoid (First (First, getFirst))

import Data.Coerce (Coercible, coerce)


-- * Modifying

type ASetter c d a b = (a -> Identity b) -> c -> Identity d
{- ^
@ASetter@ describes 'Data.Functor.fmap'-like optics used to modify and replace values.
-}

infixr 4 %~
(%~) :: ASetter c d a b -> (a -> b) -> c -> d
{- ^ @o %~ f@ ("o modified by f") applies @f@ to all values traversed by @o@.
Use '&' to pass it a data structure: @mx '&' 'Data.Traversable.traverse' %~ x@.
-}
o %~ f = runIdentity #. o (Identity #. f)
{-# INLINE (%~) #-}

infixr 4 .~
(.~) :: ASetter c d a b -> b -> c -> d
{- ^ @o .~ x@ ("o set to x") sets all values traversed by @o@ to @x@.
Use '&' to pass it a data structure: @xs '&' 'Data.Traversable.traverse' .~ x@.
-}
o .~ x = o %~ const x
{-# INLINE (.~) #-}

infixr 4 %%~
(%%~) :: ((a -> m b) -> c -> m d) -> (a -> m b) -> c -> m d
-- ^ @o %%~ f@ applies an Optic 'o' to 'f'. For example, @xs & 'Data.Traversable.traverse' %%~ f@ is just @'Data.Traversable.traverse' f xs@.
(%%~) = ($)

infixr 4 `traverseOf_`
traverseOf_ ::
    (Functor m)=> Getting (Traversed_ r m) c a -> (a -> m r) -> c -> m ()
traverseOf_ o f =
    (() <$) . getTraversed_ #. getConst #. o (Const #. Traversed_ #. f)

infixl 8 `forOf_`
forOf_ ::
    (Functor m)=> Getting (Traversed_ r m) c a -> c -> (a -> m r) -> m ()
forOf_ o s f = traverseOf_ o f s

sets :: ((a -> b) -> c -> d) -> ASetter c d a b
{- ^ Use an 'Data.Functor.fmap'-like function with '%~' or '.~':
@'Prelude.getLine' & sets 'Data.Functor.fmap' . 'Data.Traversable.traverse' '%~' 'Prelude.fromEnum' :: IO [Int]@
-}
sets f g = Identity #. f (runIdentity #. g)
{-# INLINE sets #-}


-- * Getting

type Getting r c a = (a -> Const  r a) -> c -> Const  r c
{- ^ @Getting@ is a concrete type used to instantiate optics when they are used to get single values or 'Data.Monoid.Monoid' folds.
-}

infixl 8 ^.
(^.) :: c -> Getting a c a -> a
{- ^
@x ^. o@ ("x's o") gets the value in @x@ focused on by a @Getter@ @o@.
@x ^. o@ folds together all of the 'Data.Monoid.Monoid' values in @x@ traversed by a @Traversal@ @o@.
-}
x ^. o = getConst (o Const x)
{-# INLINE (^.) #-}

infixl 8 ^?
(^?) :: c -> Getting (First a) c a -> Maybe a
-- ^ Get 'Just' the first element traveersed by a 'Getter', or 'Nothing'.
x ^? o = x ^. o . to (First #. Just) & getFirst
-- x ^? o = x ^.. o & listToMaybe
{-# INLINE (^?) #-}

(^..) :: c -> Getting (Endo [a]) c a -> [a]
-- ^ @x ^. o@ builds the list of all elements accessed by @o@.
x ^.. o = x ^. o . to singleDList & runDList
infixl 8 ^..
{-# INLINE (^..) #-}

type Getter c a = forall m. (Contravariant m)=> (a -> m a) -> c -> m c
{- ^ @Getter@ characterizes optics that can only be used to get a single value out of a structure.
-}

to :: (c -> a) -> Getter c a
{- ^ Use a function with '^.' or '^..': @
-}
to f g = contramap f . g . f
{-# INLINE to #-}


--- Module Internal Definitions (not exported) ---

runDList :: Endo [a] -> [a]
-- Convert a Hughes list (@[a] -> [a]@) to a list (@[a]@).
runDList = (`appEndo` [])
{-# INLINE runDList #-}

singleDList :: a -> Endo [a]
-- Create a singleton Hugles list.
singleDList = Endo #. (:)
{-# INLINE singleDList #-}

(#.) :: (Coercible b c)=> (b -> c) -> (a -> b) -> a -> c
-- Compose a coercion after a function.
(#.) _ = coerce
infixr 9 #.


-- | A counterpart to Const that unsafely turns Applicatives into Monoids. Do not export.
newtype Traversed_ a m = Traversed_ (m a)
getTraversed_ :: Traversed_ a m -> m a
getTraversed_ (Traversed_ ma) = ma

instance (Applicative m)=> Semigroup (Traversed_ a m) where
    (<>) = coerce ((*>) :: m a -> m a -> m a)

-- instance (Applicative m, Contravariant m)=> Monoid (Traversed_ a m) where
--     mempty = Traversed_ (() >$ pure ())

instance (Applicative m)=> Monoid (Traversed_ a m) where
    mempty = Traversed_ (pure (error "Traversed_: Do not examine `mempty`!"))
