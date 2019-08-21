{-# LANGUAGE
    NoImplicitPrelude
  , BangPatterns
  , ConstraintKinds
  , FlexibleInstances
  , MultiParamTypeClasses
  , TypeFamilies
  #-}


module XMonad.Type.Focused.Map where


import Prelude (Eq (..), Ord, otherwise)
import Control.Applicative
import Control.Category ((.))
import Control.Comonad
import Control.Lens
import Data.Functor.Apply
import Data.Foldable
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Monoid hiding (First)
import qualified Data.Map.Strict as SM
import Data.Maybe (Maybe (..))


data Map k v = Map !k !v !(SM.Map k v)

type Key = Ord

type instance Index (Map i a) = i
type instance IxValue (Map i a) = a
instance Ord i => Ixed (Map i a) where
    ix k f kvs = case lookup k kvs of
       Just v -> (\ v' -> insert k v' kvs) <$> f v
       _ -> pure kvs


instance Functor (Map k) where
    fmap f (Map k v kvs) = Map k (f v) (fmap f kvs)


instance Key i => Comonad (Map i) where
    extract (Map _ v _) = v
    -- ^ Get the focused value.
    -- duplicate kvs = imap (\k v -> refocus k v kvs) kvs
    duplicate kvs0@(Map k v kvs) = Map k kvs0 (imap refocus' kvs)
        where
        refocus' k' v' = Map k' v' (SM.delete k' kvs)
    -- duplicate kvs1@(Map k v kvs) =

instance Foldable (Map k) where
    foldMap = foldMapDefault

instance Traversable (Map k) where
    traverse f = itraverse (pure f)

instance FunctorWithIndex k (Map k)
instance FoldableWithIndex k (Map k)
instance TraversableWithIndex k (Map k) where
    itraverse f (Map k v kvs) =
        Map k <$> f k v <*> itraverse f kvs


lookup ::
    Key k =>
    k -> Map k v -> Maybe v
lookup k' (Map k v kvs)
    | k' == k = Just v
    | otherwise = SM.lookup k' kvs


singleton :: Key k => k -> v -> Map k v
singleton k v = Map k v SM.empty

insert :: Key k => k -> v -> Map k v -> Map k v
insert k' v' (Map k v kvs)
  | k' == k = Map k' v' kvs
  | otherwise = Map k v (SM.insert k' v' kvs)

refocus :: Key k => k -> v -> Map k v -> Map k v
refocus k' v' (Map k v kvs) =
    Map k' v' kvs'
    where
    kvs'
        | k' == k = kvs
        | otherwise = SM.insert k v (SM.delete k' kvs)

-- replaceFocus :: Key i => i -> a -> Map i a -> Map i a
-- ^ Delete the focus and replace it with the provided values.
-- replaceFocus k' v' (Map _ _ kvs) = Map k' v' kvs

shiftFocus :: Key k => k -> Map k v -> Maybe (Map k v)
shiftFocus k' kvs1@(Map k v kvs)
    | k' == k = Just kvs1
    | otherwise = uncheckedRefocus <$> SM.lookup k' kvs
    where
    uncheckedRefocus v' = Map k' v' (SM.insert k v kvs)
