
module XMonad.Internal.Backwards where
-- | This module defines a 'backwards' applicative using 'liftA2'. Remove it when transformers's 'Control.Applicative.Backwards' uses 'liftA2'.


import Control.Applicative


newtype Backwards m a = Backwards (m a)

forwards :: Backwards m a -> m a
forwards (Backwards mx) = mx

instance (Functor m)=> Functor (Backwards m) where
   fmap f (Backwards mx) = Backwards (fmap f mx)
   x <$ Backwards mx = Backwards (x <$ mx)

instance (Applicative m)=> Applicative (Backwards m) where
   pure x = Backwards (pure x)
   liftA2 f (Backwards mx) (Backwards my) = Backwards $ liftA2 (flip f) my mx
   (*>) = liftA2 (\ _ x -> x) -- You have to use 'liftA2' either way.

instance (Alternative m)=> Alternative (Backwards m) where
-- ^ The 'Alernative' is not backwards.
   empty = Backwards empty
   Backwards x <|> Backwards y = Backwards (x <|> y)
