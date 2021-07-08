{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module is /internal/ to xmonad. Its functions should not be re-exported, and its API may change at any point.

It provides the most basic functions for working with optics (here, 'Lens'es and 'Traversal's) to ease the modification of records and definition of composite 'Lens'es and 'Traversal's.
-}

module XMonad.Internal.Optics (
-- Modify
    (%~), (.~),
    (%%~),
    sets,
-- Summarize
    (^.), (^?), (^..),
    to,
-- Act on
    traverseOf_,
-- Read
    view,
-- Modify State
    (%=), (.=), (<~),
-- Get State
    use,
-- Traversals
    filtered, -- perhaps,
-- Helpers
    (&), (<&>), -- re-exported
    ) where

-- Classes
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Data.Coerce (Coercible, coerce)
import Data.Functor.Contravariant (Contravariant (contramap))
-- Newtypes
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Semigroup (Endo (Endo, appEndo))
import Data.Monoid (First (First, getFirst))
-- Functions
import Data.Function ((&))
import Data.Functor ((<&>))


-- ** Modifying

type ASetter c d a b = (a -> Identity b) -> c -> Identity d
{- ^
@ASetter@ describes 'Data.Functor.fmap'-like optics used to modify and replace values.

This type alias is not exported.
-}

(%~) :: ASetter c d a b -> (a -> b) -> c -> d
{- ^
@o %~ f@ ("o modified by f") applies @f@ to all values traversed by @o@.
Use '&' to pass it a data structure: @mx '&' 'Data.Traversable.traverse' %~ x@.
-}
o %~ f = runIdentity #. o (Identity #. f)
infixr 4 %~
{-# INLINE (%~) #-}

(.~) :: ASetter c d a b -> b -> c -> d
{- ^
@o .~ x@ ("o set to x") sets all values traversed by @o@ to @x@.
Use '&' to pass it a data structure: @xs '&' 'Data.Traversable.traverse' .~ x@.
-}
o .~ x = o %~ const x
infixr 4 .~
{-# INLINE (.~) #-}

(%%~) :: ((a -> m b) -> c -> m d) -> (a -> m b) -> c -> m d
{- ^
@o %%~ f@ applies an Optic @o@ to @f@. For example, @xs & 'Data.Traversable.traverse' %%~ f@ is just @'Data.Traversable.traverse' f xs@.
-}
(%%~) = ($)
infixr 4 %%~
{-# INLINE (%%~) #-}

sets :: ((a -> b) -> c -> d) -> ASetter c d a b
{- ^
Use an 'Data.Functor.fmap'-like function with '%~' or '.~':
@'Prelude.getLine' & sets 'Data.Functor.fmap' . 'Data.Traversable.traverse' '%~' 'Prelude.fromEnum' :: IO [Int]@
-}
sets f g = Identity #. f (runIdentity #. g)
{-# INLINE sets #-}


-- ** Getting

type Getting r c a = (a -> Const r a) -> c -> Const r c
{- ^
@Getting@ is a concrete type used to instantiate optics when they are used to get single values or 'Data.Monoid.Monoid' folds.

This type alias is not exported.
-}

(^.) :: c -> Getting a c a -> a
{- ^
@x ^. o@ ("x's o") gets the value in @x@ focused on by a @Getter@ @o@.
@x ^. o@ folds together all of the 'Data.Monoid.Monoid' values in @x@ traversed by a @Traversal@ @o@.
-}
x ^. o = getConst (o Const x)
infixl 8 ^.
{-# INLINE (^.) #-}

(^?) :: c -> Getting (First a) c a -> Maybe a
{- ^
Get 'Just' the first element traveersed by a 'Getter', or 'Nothing'.
-}
x ^? o = x ^. o . to (First #. Just) & getFirst
-- x ^? o = x ^.. o & listToMaybe
-- x ^? o = foldMapOf o (First #. Just) x & getFirst
infixl 8 ^?
{-# INLINE (^?) #-}

(^..) :: c -> Getting (Endo [a]) c a -> [a]
{- ^
@x ^. o@ builds the list of all elements accessed by @o@.
-}
x ^.. o = x ^. o . to singleDList & runDList
-- x ^.. o = foldMapOf o singleDList x & runDList
infixl 8 ^..
{-# INLINE (^..) #-}

traverseOf_ ::
    (Functor m)=>
    Getting (Traversed r m) c a -> (a -> m r) -> c -> m ()
{- ^ This is the optic equivalent of 'traverse_'. It only needs a 'Getting' (rather than a 'Traversal') because, like 'traverse_', it executes the effects of its traversal without (re)building a structure.
-}
traverseOf_ o f = (() <$) . getTraversed #. foldMapOf o (Traversed #. f)
-- traverseOf_ o f = (() <$) . getTraversed #. getConst #. o (Const #. Traversed #. f)
{-# INLINE traverseOf_ #-}

foldMapOf :: Getting r c a -> (a -> r) -> c -> r
{- ^ Apply a function to each item got by a 'Getting' before monadically smooshing them together.
Currently used only to define 'traverseOf_'. -}
foldMapOf o f = getConst #. o (Const #. f)
-- The subtlety here is that f is applied before the value is wrapped in Const. With 'to', the function is applied after.
{-# INLINE foldMapOf #-}

type Getter c a = forall m. (Contravariant m)=> (a -> m a) -> c -> m c
{- ^ @Getter@ characterizes optics that can only be used to get a single value out of a structure.

This type alias is not exported.
-}

to :: (c -> a) -> Getter c a
{- ^ Use a function with '^.' or '^..': @
-}
to f g = contramap f . g . f
{-# INLINE to #-}

-- ** Reading

view :: (Reader.MonadReader r m)=> Getting a r a -> m a
view o = Reader.asks (^. o)
{-# INLINE view #-}

-- ** Modifying State

infixr 4 %=
(%=) :: (State.MonadState s m)=> ASetter s s a b -> (a -> b) -> m ()
{- ^ Apply a function to part of the state.
-}
o %= f = State.modify $ o %~ f
{-# INLINE (%=) #-}

infixr 4 .=
(.=) :: (State.MonadState s m)=> ASetter s s a b -> b -> m ()
{- ^ Set part of the state to a new value.
-}
o .= x = State.modify $ o .~ x
{-# INLINE (.=) #-}

infixr 2 <~
(<~) :: (State.MonadState s m)=> ASetter s s a b -> m b -> m ()
{- ^ Think of @<~@ as @.=<<@. It sets part of the state to a monadic value.

Might be more convienient to offer (.=<<) at a lower precidence, e.g. infixr 1 to match ('=<<'). This would allow @o .=<< f =<< mx@.
-}
o <~ mx = (o .=) =<< mx
{-# INLINE (<~) #-}


-- ** Getting State

use :: (State.MonadState s m)=> Getting a s a -> m a
{- ^ Use 'State.get's part of the state focused on by a @Getting@. -}
use o = State.gets (^. o)
{-# INLINE use #-}


-- ** Specific Traversals

type Traversal c d a b = forall m. (Applicative m)=> (a -> m b) -> c -> m d

filtered :: (a -> Bool) -> Traversal a a a a
{- ^ @filtered@ traverses elements for which a predicate is 'True', and passes over (with 'pure') elements for which the predicate is 'False'.
This can violate @Traversal@ laws when the action invalidates the predicate, so use with caution.

This should be a 'Control.Lens.Prism', but we don't have 'Data.Profunctor's.
-}
filtered p f x
   | p x       = f x
   | otherwise = pure x

-- perhaps :: (a -> Maybe a) -> Traversal a a a a
-- {- ^ Create a poor man's 'Control.Lens.Prism''.

-- This either focuses on the 'Just' result or passing along the original value.
-- -}
-- perhaps g f x = maybe (pure x) f (g x)

--- Module-Internal Definitions (not exported) ---

runDList :: Endo [a] -> [a]
-- ^ Convert a Hughes list (@[a] -> [a]@) to a list (@[a]@).
runDList = (`appEndo` [])
{-# INLINE runDList #-}

singleDList :: a -> Endo [a]
-- ^ Create a singleton Hughes list.
singleDList = Endo #. (:)
{-# INLINE singleDList #-}

(#.) :: (Coercible b c)=> (b -> c) -> (a -> b) -> a -> c
-- ^ Compose a coercion after a function. @f #. g  =  'Data.Coerce.coerce' g@.
(#.) _ = coerce
infixr 9 #.
{-# INLINE (#.) #-}

newtype Traversed a m = Traversed{ getTraversed :: m a}

instance (Applicative m)=> Semigroup (Traversed a m) where
    (<>) = coerce ((*>) :: m a -> m a -> m a)
    {-# INLINE (<>) #-}

instance (Applicative m)=> Monoid (Traversed a m) where
    mempty = Traversed (pure (error "Traversed: value used"))
    {-# INLINE mempty #-}


-- ** Notes
-- (Control.Lens.<>~) is ommitted because it's not completely obvious that it's @o %~ (<> x)@ rather than @o %~ (x <>)@, and the second is sometimes what you want. Better to be explicit.
