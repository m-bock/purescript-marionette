module Test.Examples.Snake.Data.Grid
  ( Grid
  , findIndex
  , fromArrays
  , insert
  , lookup
  , toArrays
  , toMap
  ) where

import Prelude

import Data.Array as Arr
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..))
import Test.Examples.Snake.Data.Vector (Vector(..))
import Unsafe.Coerce (unsafeCoerce)

type Vec = Vector Int

data Grid a = UnsafeGrid (Vector Int) (Map Vec a)

derive instance (Eq a) => Eq (Grid a)

derive instance Functor Grid

instance Show a => Show (Grid a) where
  show = toArrays >>> show

instance Foldable Grid where
  foldr f x (UnsafeGrid _ mp) = foldr f x mp
  foldl f x (UnsafeGrid _ mp) = foldl f x mp
  foldMap f (UnsafeGrid _ mp) = foldMap f mp

instance Traversable Grid where
  traverse f (UnsafeGrid size mp) = UnsafeGrid size <$> traverse f mp
  sequence = sequenceDefault

toMap :: forall a. Grid a -> Map Vec a
toMap (UnsafeGrid _ mp) = mp

lookup :: forall a. Vec -> Grid a -> Maybe a
lookup vec (UnsafeGrid _ mp) = Map.lookup vec mp

insert :: forall a. Vec -> a -> Grid a -> Maybe (Grid a)
insert vec x (UnsafeGrid size mp) | vec < size =
  Just $ UnsafeGrid size $ Map.insert vec x mp
insert _ _ _ = Nothing

fromArrays :: forall a. Array (Array a) -> Maybe (Grid a)
fromArrays xs = do
  mp <- sequence $ map (\p -> map (Tuple p) (lookup2d p xs)) pos
  pure $ UnsafeGrid size $ Map.fromFoldable mp
  where
  pos = positionsInSize size
  size = guessSize xs

findIndex :: forall a. (a -> Boolean) -> Grid a -> Maybe Vec
findIndex = unsafeCoerce 1

index :: forall a. Int -> Array a -> Maybe a
index = flip Arr.index

toArrays :: forall a. Grid a -> Array (Array a)
toArrays (UnsafeGrid size mp) = unsafeCoerce 1

positionsInSize :: Vec -> Array Vec
positionsInSize (Vec sizex sizey) = do
  x <- Arr.range 0 (sizex - 1)
  y <- Arr.range 0 (sizey - 1)
  pure $ Vec x y

--- Array 2D

lookup2d :: forall a. Vec -> Array (Array a) -> Maybe a
lookup2d (Vec x y) = index y >=> index x

guessSize :: forall a. Array (Array a) -> Vec
guessSize xs = Vec x y
  where
  x = Arr.length <<< fromMaybe [] <<< Arr.head $ xs
  y = Arr.length xs
