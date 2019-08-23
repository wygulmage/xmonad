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
import Data.Foldable (Foldable (foldMap, foldr))
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Monoid hiding (First)
import qualified XMonad.Type.Strict.Map as SM
import Data.Either (Either (..))
import Data.Maybe (Maybe (..), maybe, fromMaybe)


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
    duplicate kvs0@(Map k _ kvs) = Map k kvs0 (imap newFocus' kvs)
        where
        newFocus' k' v' = Map k' v' (SM.delete k' kvs)

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

--- Query ---

lookupWith :: Ord i => b -> (a -> b) -> i -> Map i a -> b
lookupWith z f k' (Map k v kvs)
    | k' == k = f v
    | otherwise = SM.lookupWith z f k' kvs

lookup ::
    Key k =>
    k -> Map k v -> Maybe v
lookup k' (Map k v kvs)
    | k' == k = Just v
    | otherwise = SM.lookup k' kvs

--- Modify ---

insertWith ::
    Ord i =>
    (a -> a -> a) -> i -> a -> Map i a -> Map i a
insertWith f k' v' (Map k v kvs)
    | k' == k = Map k (f v' v) kvs
    | otherwise = Map k v (SM.insertWith f k' v' kvs)

insert :: (Key k, Semigroup v) => k -> v -> Map k v -> Map k v
insert = insertWith (<>)

replace :: Ord i => i -> a -> Map i a -> Map i a
replace = insertWith pure

delete ::
    Ord i =>
    i -> Map i a -> Either (SM.Map i a) (Map i a)
delete k' (Map k v kvs)
    | k' == k = Left kvs
    | otherwise = Right (Map k v (SM.delete k' kvs))

pop ::
    Ord i =>
    i -> Map i a -> Maybe (a, Either (SM.Map i a) (Map i a))
-- If k is not in kvs, Nothing
-- If k is the focus, return Just the value of the focus and the unfocused map.
-- Otherwise return Just the value at k and the focused map without k.
pop k kvs = (\ v -> (v, delete k kvs)) <$> lookup k kvs

--- Change Focus ---

focusOn :: Key i => i -> SM.Map i v -> Maybe (Map i v)
focusOn k kvs = uncurry (Map k) <$> SM.pop k kvs

newFocusWith ::
    Ord i =>
    (a -> a -> a) -> i -> a -> Map i a -> Map i a
newFocusWith f k' v' (Map k v kvs)
    | k' == k = Map k (f v' v) kvs
    | otherwise = Map k' v'' kvs'
       where
       (v'', kvs') = fromMaybe (v', kvs) (SM.pop k' kvs)

newFocus ::
    (Ord i, Semigroup a) =>
    i -> a -> Map i a -> Map i a
newFocus = newFocusWith (<>)

-- replaceFocus :: Key i => i -> a -> Map i a -> Map i a
-- ^ Delete the focus and replace it with the provided values.
-- replaceFocus k' v' (Map _ _ kvs) = Map k' v' kvs

shiftFocus ::
    Key k =>
    k -> Map k v -> Maybe (Map k v)
shiftFocus k' kvs1@(Map k v kvs)
    | k' == k = Just kvs1
    | otherwise = uncheckedNewFocus <$> SM.lookup k' kvs
    where
    uncheckedNewFocus v' = Map k' v' (SM.replace k v kvs)

unFocus :: Ord i => Map i a -> SM.Map i a
unFocus (Map k v kvs) = SM.replace k v kvs

--- Combine ---

-- Use (<>) for 'union'.

unionWith :: Ord i => (a -> a -> a) -> Map i a -> Map i a -> Map i a
-- Keeps the left focus,
unionWith f (Map k v kvs) (Map j u jus)
    | k == j = Map k (f v u) (SM.unionWith f kvs jus)
    | otherwise = Map k v' (SM.unionWith f kvs jus')
         where
         (v', jus') = maybe (v, jus) (first (f v)) (SM.pop k jus)
