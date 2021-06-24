{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XMonad.Internal.Backwards where
-- | This module defines a 'backwards' applicative using 'liftA2'. Remove it when transformers's 'Control.Applicative.Backwards' uses 'liftA2'. It only defines the instances necessary for its internal use.


import Control.Applicative


newtype Backwards m a = Backwards (m a)
  deriving (Functor, Alternative)

forwards :: Backwards m a -> m a
forwards (Backwards mx) = mx

instance (Applicative m)=> Applicative (Backwards m) where
   pure x = Backwards (pure x)
   {-# INLINE pure #-}
   liftA2 f (Backwards mx) (Backwards my) = Backwards (liftA2 (flip f) my mx)
   {-# INLINE liftA2 #-}
   (*>) = liftA2 (\ _ x -> x) -- You have to use 'liftA2' either way.
   {-# INLINE (*>) #-}
