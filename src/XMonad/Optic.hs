{-# LANGUAGE
    RankNTypes
  #-}

module XMonad.Optic where

import Data.Functor.Const (Const (Const), getConst)
import Data.Functor.Identity (Identity (Identity), runIdentity)

------- Types -------

type Lens s t a b = forall m. Functor m => (a -> m b) -> s -> m t
type MonoLens s a = Lens s s a a

------- Functions -------

views :: ((a -> Const c b) -> s -> Const c t) -> (a -> c) -> s -> c
views l f = getConst . l (Const . f)

view :: ((a -> Const a b) -> s -> Const a s) -> s -> a
view l = views l id

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set l = over l . const

