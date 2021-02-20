
module XMonad.Internal.Optic
   ((%%~), (%~), (.~), sets, (^.), to)
   where

import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Coerce (Coercible, coerce)

(%%~) :: o -> o
(%%~) = id
infixr 4 %%~
{-# inline (%%~) #-}

(%~) :: ((a -> Identity b) -> c -> Identity d) -> (a -> b) -> c -> d
o %~ f = runIdentity #. o (Identity #. f)
infixr 4 %~
{-# INLINE (%~) #-}

(.~) :: ((a -> Identity b) -> c -> Identity d) -> b -> c -> d
o .~ x = runIdentity #. o (\_-> Identity x)
infixr 4 .~
{-# INLINE (.~) #-}

sets :: ((a -> b) -> c -> d) -> (a -> Identity b) -> c -> Identity d
sets f g = Identity #. f (runIdentity #. g)
{-# INLINE sets #-}

(^.) :: c -> ((a -> Const a a) -> c -> Const a c) -> a
x ^. o = getConst (o Const x)
infixl 8 ^.
{-# INLINE (^.) #-}

to :: (Contravariant m)=> (a -> b) -> (b -> m b) -> a -> m a
to f g = contramap f . g . f
{-# INLINE to #-}

(#.) :: (Coercible c b)=> (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
infixr 9 #.
{-# INLINE (#.) #-}
