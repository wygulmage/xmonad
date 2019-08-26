{-# LANGUAGE
    NoImplicitPrelude
  , ConstraintKinds
  , FlexibleInstances
  , MultiParamTypeClasses
  , TypeFamilies
  #-}


module XMonad.Type.Focused.Map where


import Prelude (Eq (..), Ord, fst, snd, uncurry, otherwise)
import Control.Applicative
import Control.Category ((.))
import Control.Comonad
import Control.Lens
import Data.Bifunctor (first)
import Data.Functor.Apply
import Data.Foldable (Foldable (foldMap, foldr))
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Monoid hiding (First)
import qualified XMonad.Type.Strict.Map as SM
import Data.Either (Either (..))
import Data.Maybe (Maybe (..), maybe, fromMaybe)


data Map i a = Map !i !a !(SM.Map i a) (SM.Map i a)


------- Instances -------

type instance Index (Map i a) = i
type instance IxValue (Map i a) = a
instance (Key i, Semigroup a) => Ixed (Map i a) where
    ix k f kvs = case lookup k kvs of
       Just v -> (\ v' -> insert k v' kvs) <$> f v
       _ -> pure kvs

-- instance Ord i => Apply (Map i) where
    -- liftA2 f (Map k v kvLs kvRs) (Map j u juLs juRs)
        -- | k == j
