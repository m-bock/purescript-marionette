module Test.Examples.Snake.Data.Grid
  ( ErrorFromArrays(..)
  , Grid
  , Vec
  , coords
  , empty
  , fill
  , findEntry
  , fromArray
  , fromArrays
  , insert
  , insert'
  , insertSubgrid
  , insertSubgridCropped
  , isInSize
  , lookup
  , moduloLookup
  , size
  , toArrays
  , toMap
  , toUnfoldable
  )
  where

import Prelude

import Data.Array (foldM)
import Data.Array as Arr
import Data.Either (Either, note)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable, all, foldMap, foldl, foldr, sequence, sequenceDefault, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Data.Unfoldable (class Unfoldable)
import Debug (spy)
import Partial.Unsafe (unsafeCrashWith)
import Test.Examples.Snake.Data.Vector (Vector(..))
import Unsafe.Coerce (unsafeCoerce)

type Vec = Vector Int

data Grid a = UnsafeGrid (Vector Int) (Map Vec a)

derive instance (Eq a) => Eq (Grid a)

derive instance Functor Grid

instance FunctorWithIndex Vec Grid where
  mapWithIndex f (UnsafeGrid size mp) = UnsafeGrid size $ mapWithIndex f mp

instance Show a => Show (Grid a) where
  show grid = show { size: size grid, entries: toArrays grid }

instance Foldable Grid where
  foldr f x (UnsafeGrid _ mp) = foldr f x mp
  foldl f x (UnsafeGrid _ mp) = foldl f x mp
  foldMap f (UnsafeGrid _ mp) = foldMap f mp

instance FoldableWithIndex Vec Grid where
  foldrWithIndex f x (UnsafeGrid size mp) = foldrWithIndex f x mp
  foldlWithIndex f x (UnsafeGrid size mp) = foldlWithIndex f x mp
  foldMapWithIndex = foldMapWithIndexDefaultL

instance Traversable Grid where
  traverse f (UnsafeGrid size mp) = UnsafeGrid size <$> traverse f mp
  sequence = sequenceDefault

instance TraversableWithIndex Vec Grid where
  traverseWithIndex f (UnsafeGrid size mp) = UnsafeGrid size <$> traverseWithIndex f mp

findEntry :: forall a. ((Vec /\ a) -> Boolean) -> Grid a -> Maybe (Vec /\ a)
findEntry f grid = toUnfoldable grid # Arr.find f

empty :: forall a. Grid a
empty = UnsafeGrid zero Map.empty

fill :: forall a. (Vec -> a) -> Vec -> Grid a -- TODO handle minus
fill f size = positionsInSize size
  <#> (\k -> Tuple k (f k))
  # Map.fromFoldable
  # UnsafeGrid size

positionsInSize :: Vec -> Array Vec
positionsInSize (Vec w h) = ado
  x <- Arr.range 0 (w - 1)
  y <- Arr.range 0 (h - 1)
  in Vec x y

coords :: forall a. Grid a -> Set Vec
coords (UnsafeGrid _ mp) = Map.keys mp

toUnfoldable :: forall a f. Unfoldable f => Grid a -> f (Vec /\ a)
toUnfoldable (UnsafeGrid _ mp) = Map.toUnfoldable mp

toMap :: forall a. Grid a -> Map Vec a
toMap (UnsafeGrid _ mp) = mp

lookup :: forall a. Vec -> Grid a -> Maybe a
lookup vec (UnsafeGrid _ mp) = Map.lookup vec mp

moduloLookup :: forall a. Vec -> Grid a -> a
moduloLookup vec grid = lookup vecSafe grid
  # fromMaybe' (\_ -> unsafeCrashWith "Modulo lookup")
  where
  vecSafe = mod <$> vec <*> size grid

insert :: forall a. Vec -> a -> Grid a -> Maybe (Grid a)
insert vec x (UnsafeGrid size mp) | isInSize vec size =
  Just $ UnsafeGrid size $ Map.insert vec x mp
insert _ _ _ = Nothing

insert' :: forall a. Vec -> a -> Grid a -> Grid a
insert' x1 x2 grid = insert x1 x2 grid # fromMaybe grid

isInSize :: Vec -> Vec -> Boolean
isInSize vec size =
  all identity $ inRange <$> vec <*> size
  where
  inRange :: Int -> Int -> Boolean
  inRange x w = 0 <= x && x < w

size :: forall a. Grid a -> Vec
size (UnsafeGrid size _) = size

insertSubgrid :: forall a. Vec -> Grid a -> Grid a -> Maybe (Grid a)
insertSubgrid vec src tgt = tgt
  # toUnfoldable
  # foldM (\grid (Tuple k v) -> insert (vec + k) v grid) src

insertSubgridCropped :: forall a. Vec -> Grid a -> Grid a -> Grid a
insertSubgridCropped vec src tgt = src
  # (toUnfoldable :: _ -> Array _)
  # foldl (\grid (Tuple k v) -> fromMaybe grid $ insert (vec + k) v grid) tgt

data ErrorFromArrays = ErrLineWrongLength { guessedSize :: Vec, pos :: Vec }

fromArrays :: forall a. Array (Array a) -> Either ErrorFromArrays (Grid a)
fromArrays xs = do
  mp <- sequence $ pos <#> \p ->
    map (Tuple p) (lookup2d p xs)
      # note (ErrLineWrongLength { guessedSize: size, pos: p })
  pure $ UnsafeGrid size $ Map.fromFoldable mp
  where
  pos = positionsInSize size
  size = guessSize xs

fromArray :: forall a. Array a -> Grid a
fromArray xs = UnsafeGrid size mp
  where
  size = Vec (Arr.length xs) 1
  mp = xs # Arr.mapWithIndex (\x v -> Tuple (Vec x 0) v) # Map.fromFoldable

index :: forall a. Int -> Array a -> Maybe a
index = flip Arr.index

toArrays :: forall a. Grid a -> Array (Array a)
toArrays (UnsafeGrid (Vec w h) mp) =
  Arr.range 0 (h - 1) <#> mkLine
  where
  mkLine y = Arr.range 0 (w - 1) <#> \x -> mkCell x y
  mkCell x y = Map.lookup (Vec x y) mp # fromMaybe'
    \_ -> unsafeCrashWith "Impossible lookup"

--- Array 2D

lookup2d :: forall a. Vec -> Array (Array a) -> Maybe a
lookup2d (Vec x y) = index y >=> index x

guessSize :: forall a. Array (Array a) -> Vec
guessSize xs = Vec x y
  where
  x = Arr.length <<< fromMaybe [] <<< Arr.head $ xs
  y = Arr.length xs

---

derive instance Eq ErrorFromArrays

derive instance Generic ErrorFromArrays _

instance Show ErrorFromArrays where
  show = genericShow
