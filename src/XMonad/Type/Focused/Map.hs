{-# LANGUAGE
    NoImplicitPrelude
  , BangPatterns
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
import Data.Foldable
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Monoid hiding (First)
import qualified XMonad.Type.Strict.Map as SM
import Data.Maybe (Maybe (..), maybe)


data Map k v = Map !k !v !(SM.Map k v)

type Key = Ord


------- Instances -------

type instance Index (Map i a) = i
type instance IxValue (Map i a) = a
instance (Key i, Semigroup a) => Ixed (Map i a) where
    ix k f kvs = case lookup k kvs of
       Just v -> (\ v' -> insert k v' kvs) <$> f v
       _ -> pure kvs


instance Key k => Functor (Map k) where
    fmap f (Map k v kvs) = Map k (f v) (fmap f kvs)


instance Key i => Comonad (Map i) where
    extract (Map _ v _) = v
    -- ^ Get the focused value.
    duplicate kvs0@(Map k _ kvs) = Map k kvs0 (imap refocus' kvs)
        where
        refocus' k' v' = Map k' v' (SM.delete k' kvs)

instance Key k => Foldable (Map k) where
    foldMap = foldMapDefault

instance Key k =>  Traversable (Map k) where
    traverse f = itraverse (pure f)

instance Key k => FunctorWithIndex k (Map k)
instance Key k => FoldableWithIndex k (Map k)
instance Key k => TraversableWithIndex k (Map k) where
    itraverse f (Map k v kvs) =
        Map k <$> f k v <*> itraverse f kvs


------- Functions -------

lookup ::
    Key k =>
    k -> Map k v -> Maybe v
lookup k' (Map k v kvs)
    | k' == k = Just v
    | otherwise = SM.lookup k' kvs

insert :: (Key k, Semigroup v) => k -> v -> Map k v -> Map k v
insert k' v' (Map k v kvs)
  | k' == k = Map k' v' kvs
  | otherwise = Map k v (SM.insert k' v' kvs)

focusMapOn :: Key i => i -> SM.Map i v -> Maybe (Map i v)
focusMapOn k kvs = uncurry (Map k) <$> SM.pop k kvs

refocus :: (Key k, Semigroup v) => k -> v -> Map k v -> Map k v
refocus k' v' (Map k v kvs) =
    Map k' v' kvs'
    where
    kvs'
        | k' == k = kvs
        | otherwise = SM.insert k v (SM.delete k' kvs)

-- replaceFocus :: Key i => i -> a -> Map i a -> Map i a
-- ^ Delete the focus and replace it with the provided values.
-- replaceFocus k' v' (Map _ _ kvs) = Map k' v' kvs

shiftFocus ::
    (Key k, Semigroup v) =>
    k -> Map k v -> Maybe (Map k v)
shiftFocus k' kvs1@(Map k v kvs)
    | k' == k = Just kvs1
    | otherwise = uncheckedRefocus <$> SM.lookup k' kvs
    where
    uncheckedRefocus v' = Map k' v' (SM.insert k v kvs)


unionWith :: Ord i => (a -> a -> a) -> Map i a -> Map i a -> Map i a
-- Keeps the left focus,
unionWith f (Map k v kvs) (Map j u jus)
    | k == j = Map k (f v u) (SM.unionWith f kvs jus)
    | otherwise = Map k v' (SM.unionWith f kvs jus')
         where
         mujus' = SM.pop k jus
         (v', jus') = maybe (v, jus) (first (f v)) mujus'
         -- v' = maybe v ((<>) v . fst) mujus'
         -- jus' = maybe jus snd mujus'
