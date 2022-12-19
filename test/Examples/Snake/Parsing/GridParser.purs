module Test.Examples.Snake.Parsing.GridParser
  ( GridParseError(..)
  , GridParser
  , any
  , moveTo
  , position
  , runParser
  , satisfies
  , scanGrid
  , setDirection
  ) where

import Prelude

import Control.Monad.State (State, modify_, runState)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), snd)
import Test.Examples.Snake.Data.Direction (Direction)
import Test.Examples.Snake.Data.Direction as Dir
import Test.Examples.Snake.Data.Grid (Grid, Vec)
import Test.Examples.Snake.Data.Grid as Grid
import Unsafe.Coerce (unsafeCoerce)

type Context a =
  { direction :: Direction
  , position :: Vec
  , grid :: Grid (Maybe a)
  }

getCurrent :: forall a. Context a -> Maybe (Maybe a)
getCurrent { grid, position } = Grid.lookup position grid

next :: forall a. Context a -> Context a
next ctx = ctx { position = ctx.position + Dir.toVector ctx.direction }

newtype GridParser a b = GridParser (Context a -> Either GridParseError (Tuple (Context a) b))

type GridPrinter a b = b -> Grid a -> Grid a

instance Functor (GridParser a) where
  map f (GridParser g) = GridParser \ctx -> case g ctx of
    Left err -> Left err
    Right res -> Right $ map f res

instance Apply (GridParser a) where
  apply (GridParser h) (GridParser g) = GridParser \ctx ->
    case h ctx of
      Left err -> Left err
      Right (Tuple ctx' f) -> case g ctx' of
        Left err -> Left err
        Right (Tuple ctx'' x) -> Right $ Tuple ctx'' (f x)

instance Applicative (GridParser a) where
  pure x = GridParser \ctx -> Right (Tuple ctx x)

runParser' :: forall a b. Grid a -> GridParser a b -> Either GridParseError (Tuple (Context a) b)
runParser' grid (GridParser f) =
  f
    { direction: Dir.Right
    , position: zero
    , grid: Just <$> grid
    }

runParser :: forall a b. Show a => Grid a -> GridParser a b -> Either GridParseError b
runParser x1 x2 = runParser' x1 x2 <#> snd

data GridParseError = GridParseError String

derive instance Eq GridParseError

derive instance Generic GridParseError _

instance Show GridParseError where
  show = genericShow

scanGrid :: forall b a. a -> (Vec -> b -> Maybe a) -> GridParser b (Grid a)
scanGrid init f = GridParser \ctx ->
  let
    Tuple newGrid outGrid = initialGrid ctx
      # runState (traverseWithIndex go ctx.grid)
    ctx' = ctx { grid = newGrid }
  in
    Right $ Tuple ctx' outGrid
  where
  initialGrid :: _ -> Grid a
  initialGrid ctx = Grid.fill (const init) (Grid.size ctx.grid)

  go :: Vec -> Maybe b -> State (Grid a) (Maybe b)
  go k v = case v of
    Nothing -> pure Nothing
    Just v' -> case f k v' of
      Nothing -> pure v
      Just x -> do
        modify_ (\g -> Grid.insert k x g # fromMaybe g)
        pure Nothing

setDirection :: forall b a. Direction -> GridParser b a
setDirection = unsafeCoerce 1

satisfies :: forall b. (Vec -> b -> Boolean) -> GridParser b b
satisfies f = GridParser \ctx ->
  let
    cur = getCurrent ctx
  in
    case cur of
      Nothing -> Left $ GridParseError "eof"
      Just Nothing -> Left $ GridParseError "already consumed"
      Just (Just x) ->
        if f ctx.position x then
          Right $ Tuple (next ctx) x
        else Left $ GridParseError "sat2"

any :: forall b. GridParser b b
any = satisfies (\_ _ -> true)

moveTo :: forall b. (Vec -> b -> Boolean) -> GridParser b Unit
moveTo f = GridParser \ctx ->
  case Grid.findEntry f' ctx.grid of
    Nothing -> Left $ GridParseError "move error"
    Just (Tuple pos _) -> Right $ Tuple (ctx { position = pos }) unit
  where
  f' (Tuple k v) = case v of
    Nothing -> false
    Just x -> f k x

position :: forall b. GridParser b Vec
position = GridParser \ctx -> Right $ Tuple ctx ctx.position

