module Test.Examples.Snake.Data.Vector
  ( Vector(..)
  , _x
  , _y
  , one_x
  , one_y
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (class Foldable, foldMapDefaultR, foldlDefault)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

--- Vector

data Vector a = Vec a a

instance Semiring a => Semiring (Vector a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance Ring a => Ring (Vector a) where
  sub = lift2 sub

derive instance Eq a => Eq (Vector a)

derive instance Ord a => Ord (Vector a)

derive instance Functor Vector

derive instance Generic (Vector a) _

instance Show a => Show (Vector a) where
  show = genericShow

instance Applicative Vector where
  pure x = Vec x x

instance Apply Vector where
  apply (Vec f g) (Vec x y) = Vec (f x) (g y)

instance Foldable Vector where
  foldr f z (Vec x y) = f y $ f x z
  foldl f z (Vec x y) = f (f z x) y
  foldMap = foldMapDefaultR

one_y :: forall a. Semiring a => Vector a
one_y = Vec zero one

one_x :: forall a. Semiring a => Vector a
one_x = Vec one zero

_x :: forall a. Vector a -> a
_x (Vec x _) = x

_y :: forall a. Vector a -> a
_y (Vec _ y) = y

vec :: forall a. a -> a -> Vector a
vec = Vec