{-# LANGUAGE
    NoImplicitPrelude
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeFamilies
  #-}

module XMonad.Type.Strict.Map where


import Prelude (Ord, Read, Show, Bool (..), fromIntegral)
import Control.Applicative
import Control.Category
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Functor
import Data.Functor.Apply
import Data.Foldable
-- import Data.Traversable
import Data.Semigroup
import Data.Monoid
import Data.Maybe (Maybe (..), maybe)
import qualified Data.Map.Strict as SM
import Numeric.Natural (Natural)


newtype Map i a = Map{ getMap :: SM.Map i a } deriving (Show, Read, NFData)


------- Instances -------

type instance Index (Map i a) = i
type instance IxValue (Map i a) = a

instance (Ord i, Semigroup a) => Ixed (Map i a) where
    ix k f kvs = case lookup k kvs of
        Just v -> (\ v' -> insert k v' kvs) <$> f v
        _ -> pure kvs

instance (Ord i, Semigroup a) => At (Map i a) where
    at k f kvs = alter <$> f mv
        where
        mv = lookup k kvs
        alter (Just v) = insert k v kvs
        alter Nothing = maybe kvs (pure (delete k kvs)) mv



instance (Ord i, Semigroup a) => Semigroup (Map i a) where
    (<>) = unionWith (<>)

instance (Ord i, Semigroup a) => Monoid (Map i a) where
    mempty = Map SM.empty

instance Ord i => Functor (Map i) where
    fmap f = Map . fmap f . getMap

instance Ord i => Foldable (Map i) where
    foldMap f = foldMap f . getMap
    foldr f z = foldr f z . getMap

instance Ord i => Traversable (Map i) where
    traverse f = fmap Map . traverse f . getMap

instance Ord i => FunctorWithIndex i (Map i) where
    imap f = Map . imap f . getMap

instance Ord i => FoldableWithIndex i (Map i) where
    ifoldMap f = ifoldMap f . getMap
    ifoldr f z = ifoldr f z . getMap

instance Ord i => TraversableWithIndex i (Map i) where
    itraverse f = fmap Map . itraverse f . getMap

instance Ord i => Apply (Map i) where
    liftF2 f (Map kvs) = Map . SM.intersectionWith f kvs . getMap


------- Functions -------

-- For empty use mempty. You didn't want to use a non-Semigroup Map anyway, right?

singleton :: i -> a -> Map i a
singleton k = Map . SM.singleton k

null :: Map i a -> Bool
null = SM.null . getMap

size :: Map i a -> Natural
size = fromIntegral . SM.size . getMap

lookup :: Ord i => i -> Map i a -> Maybe a
lookup k = SM.lookup k . getMap

findWithDefault :: Ord i => a -> i -> Map i a -> a
findWithDefault v k = SM.findWithDefault v k . getMap

insert :: (Ord i, Semigroup a) => i -> a -> Map i a -> Map i a
insert k v = Map . SM.insertWith (<>) k v . getMap

delete :: Ord i => i -> Map i a -> Map i a
delete k = Map . SM.delete k . getMap

pop :: Ord i => i -> Map i a -> Maybe (a, Map i a)
pop k kvs = (\ v -> (v, delete k kvs)) <$> lookup k kvs
-- pop k kvs = case lookup k kvs of
    -- Just v -> Just (v, delete k kvs)
    -- _ -> Nothing


-- for 'union' use '(<>)'.

unionWith :: Ord i => (a -> a -> a) -> Map i a -> Map i a -> Map i a
unionWith f (Map kvs)  = Map . SM.unionWith f kvs . getMap

-- for 'intersectionWith' use 'liftF2'.

intersection :: (Ord i, Semigroup a) => Map i a -> Map i a -> Map i a
intersection = liftF2 (<>)

differenceWith :: Ord i => (a -> b -> Maybe a) -> Map i a -> Map i b -> Map i a
differenceWith f (Map kvs) =
    Map . SM.differenceWith f  kvs. getMap
