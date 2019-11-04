
module XMonad.Type.RationalRect where

-- | A structure for window geometries
data RationalRect = RationalRect !Rational !Rational !Rational !Rational
    deriving (Show, Read, Eq)
