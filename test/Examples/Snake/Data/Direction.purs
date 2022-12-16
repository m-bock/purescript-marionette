module Test.Examples.Snake.Data.Direction where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum, enumFromTo)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.Examples.Snake.Data.Vector (Vector)
import Test.Examples.Snake.Data.Vector as Vec

data Direction = Up | Right | Down | Left

derive instance Generic Direction _

derive instance Eq Direction
derive instance Ord Direction

instance Bounded Direction where
  top = genericTop
  bottom = genericBottom

instance Show Direction where
  show = genericShow

instance Enum Direction where
  succ = genericSucc
  pred = genericPred

instance BoundedEnum Direction where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

reverse :: Direction -> Direction
reverse = case _ of
  Up -> Down
  Right -> Left
  Down -> Up
  Left -> Right

directionsClockwise :: Array Direction
directionsClockwise = enumFromTo bottom top

toVector :: Direction -> Vector Int
toVector = case _ of
  Left -> zero - Vec.one_x
  Right -> zero + Vec.one_x
  Up -> zero - Vec.one_y
  Down -> zero + Vec.one_y
