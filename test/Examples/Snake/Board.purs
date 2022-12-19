module Test.Examples.Snake.Board
  ( Board
  , BoardEvent(..)
  , Goodie(..)
  , ParseError(..)
  , RandInt(..)
  , Tile(..)
  , class ToGrid
  , runBoard
  , parse
  , setDirection
  , toGrid
  ) where

import Prelude

import Data.Array as Arr
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (foldr, traverse)
import Data.Tuple (Tuple(..), fst)
import Debug (spy)
import Test.Examples.Snake.Data.CharGrid as CharGrid
import Test.Examples.Snake.Data.Direction (Direction)
import Test.Examples.Snake.Data.Direction as Dir
import Test.Examples.Snake.Data.Grid (ErrorFromArrays, Grid, Vec)
import Test.Examples.Snake.Data.Grid as Grid
import Test.Examples.Snake.Parsing.GridParser (GridParseError, GridParser)
import Test.Examples.Snake.Parsing.GridParser as GP

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data Board = UnsafeBoard Board'

type Board' =
  { maze :: Maze
  , snake :: Snake
  , goodie :: Goodie
  , direction :: Direction
  }

newtype Maze = Maze (Grid MazeTile)

data Snake = Snake Vec (NonEmptyArray Vec)

newtype Goodie = Goodie Vec

data Tile
  = Tile_SnakeHead
  | Tile_SnakeBody
  | Tile_Goodie
  | Tile_Wall
  | Tile_Floor

data MazeTile
  = MazeTile_Floor
  | MazeTile_Wall

newtype RandInt = RandInt Int

data BoardEvent
  = Event_Continue Board
  | Event_EatGoodie Board
  | Event_Collision
  | Event_NoSpaceLeft

--------------------------------------------------------------------------------
--- ToVecs
--------------------------------------------------------------------------------

class ToVecs a where
  toVecs :: a -> Set Vec

instance ToVecs Snake where
  toVecs (Snake head tail) = Set.singleton head <> Set.fromFoldable tail

instance ToVecs Maze where
  toVecs (Maze grid) = Grid.coords grid

--------------------------------------------------------------------------------
--- MapVec
--------------------------------------------------------------------------------

class MapVec a where
  mapVec :: (Vec -> Vec) -> a -> a

instance MapVec Snake where
  mapVec f (Snake head tail) = Snake (f head) (map f tail)

--------------------------------------------------------------------------------
--- Core
--------------------------------------------------------------------------------

unBoard :: Board -> Board'
unBoard (UnsafeBoard x) = x

runBoard :: RandInt -> Board -> BoardEvent
runBoard randInt oldBoard@(UnsafeBoard oldBoard'@{ direction, maze }) =
  let
    oldGrid = toGrid oldBoard

    Snake oldSnakeHead _ = oldBoard'.snake

    vecCandidate = oldSnakeHead + Dir.toVector direction

    tileCandidate = Grid.moduloLookup vecCandidate oldGrid

  in
    case tileCandidate of
      Tile_Floor -> Event_Continue $
        UnsafeBoard oldBoard'
          { snake = oldBoard'.snake
              # moveSnake direction
              # mapVec (moduloMaze oldGrid)
          }

      Tile_Goodie ->
        let
          newSnake = oldBoard'.snake
            # extendSnake direction
            # mapVec (moduloMaze oldGrid)

          freeSpots = getFreeSpots maze newSnake

          xs = Set.toUnfoldable freeSpots

          idx = mod (unwrap randInt) (Arr.length xs)
        in
          case Goodie <$> Arr.index xs idx of
            Just newGoodie -> Event_EatGoodie $
              UnsafeBoard oldBoard'
                { snake = newSnake
                , goodie = newGoodie
                }
            Nothing -> Event_NoSpaceLeft

      Tile_SnakeHead -> Event_Collision

      Tile_SnakeBody -> Event_Collision

      Tile_Wall -> Event_Collision

  where
  moduloMaze grid x = mod <$> x <*> Grid.size grid

moveSnake :: Direction -> Snake -> Snake
moveSnake dir (Snake snakeHead snakeTail) =
  Snake (snakeHead + vec) (NEA.cons' snakeHead $ NEA.init snakeTail)
  where
  vec = Dir.toVector dir

extendSnake :: Direction -> Snake -> Snake
extendSnake dir (Snake snakeHead snakeTail) =
  Snake (snakeHead + vec) (NEA.cons snakeHead snakeTail)
  where
  vec = Dir.toVector dir

getFreeSpots :: Maze -> Snake -> Set Vec
getFreeSpots (Maze mazeGrid) snake = Set.difference mazeVecs (toVecs snake)
  where
  mazeVecs = mazeGrid
    # Grid.toUnfoldable
    # Arr.filter (\(Tuple k v) -> v == MazeTile_Floor)
    <#> fst
    # Set.fromFoldable

setDirection :: Direction -> Board -> Maybe Board
setDirection direction (UnsafeBoard board@{ snake: Snake snakeHead snakeTail }) =
  if snakeHead + Dir.toVector direction == NEA.head snakeTail then
    Nothing
  else
    Just $ UnsafeBoard board { direction = direction }

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive instance Newtype RandInt _

derive instance Generic ParseError _

derive instance Generic Tile _

derive instance Eq Board

derive instance Eq Goodie

derive instance Eq Maze

derive instance Eq MazeTile

derive instance Eq Snake

derive instance Eq ParseError

derive instance Eq Tile

instance Show Board where
  show (UnsafeBoard i) = "board:" -- <> show i

instance Show ParseError where
  show = genericShow

instance Show Tile where
  show = genericShow

--------------------------------------------------------------------------------
--- Parser
--------------------------------------------------------------------------------

data ParseError
  = ErrFromArrays ErrorFromArrays
  | ErrInvalidChar Char
  | ErrGridParse GridParseError

parse :: String -> Either ParseError Board
parse str = do
  charBoard <- lmap ErrFromArrays $ CharGrid.fromString str
  tileBoard <- traverse parseChar charBoard
  lmap ErrGridParse $ GP.runParser tileBoard fromGrid

parseChar :: Char -> Either ParseError Tile
parseChar = case _ of
  '#' -> Right Tile_Wall
  'O' -> Right Tile_SnakeBody
  '+' -> Right Tile_SnakeHead
  'x' -> Right Tile_Goodie
  ' ' -> Right Tile_Floor
  c -> Left $ ErrInvalidChar c

--------------------------------------------------------------------------------
--- FromGrid
--------------------------------------------------------------------------------

class FromGrid a where
  fromGrid :: GridParser Tile a

instance FromGrid Board where
  fromGrid = ado
    maze <- fromGrid
    snake <- fromGrid
    goodie <- fromGrid
    direction <- fromGrid
    parseRest
    in UnsafeBoard { maze, snake, goodie, direction }

instance FromGrid Maze where
  fromGrid = Maze <$> GP.scanGrid MazeTile_Floor \_ -> case _ of
    Tile_Wall -> Just MazeTile_Wall
    _ -> Nothing

instance FromGrid Snake where
  fromGrid = ado
    --GP.setDirection Dir.Right
    GP.moveTo (\_ -> (_ == Tile_SnakeHead))
    h <- GP.position <* GP.any
    b1 <- GP.position <* GP.satisfies (\_ -> (_ == Tile_SnakeBody))
    b2 <- GP.position <* GP.satisfies (\_ -> (_ == Tile_SnakeBody))
    b3 <- GP.position <* GP.satisfies (\_ -> (_ == Tile_SnakeBody))
    b4 <- GP.position <* GP.satisfies (\_ -> (_ == Tile_SnakeBody))
    in Snake h (NEA.cons' b1 [ b2, b3, b4 ])

instance FromGrid Goodie where
  fromGrid = ado
    GP.moveTo (\_ -> (_ == Tile_Goodie))
    g <- GP.position <* GP.any
    in Goodie g

instance FromGrid Direction where
  fromGrid = pure Dir.Left

parseRest :: GridParser Tile Unit
parseRest = void $ GP.scanGrid unit \_ -> case _ of
  Tile_Wall -> Just unit
  _ -> Nothing

--------------------------------------------------------------------------------
--- ToGrid
--------------------------------------------------------------------------------

class ToGrid a where
  toGrid :: a -> Grid Tile

instance ToGrid Board where
  toGrid = unBoard >>> \{ maze, goodie, snake } ->
    toGrid maze
      # gridBuilder goodie
      # gridBuilder snake

instance ToGrid Maze where
  toGrid (Maze grid) = grid <#> case _ of
    MazeTile_Wall -> Tile_Wall
    MazeTile_Floor -> Tile_Floor

--------------------------------------------------------------------------------
--- GridBuilder
--------------------------------------------------------------------------------

class GridBuilder a where
  gridBuilder :: a -> Grid Tile -> Grid Tile

instance GridBuilder Snake where
  gridBuilder (Snake snakeHead snakeTail) grid = grid
    # Grid.insert' snakeHead Tile_SnakeHead
    # \g -> foldr (\pos -> Grid.insert' pos Tile_SnakeBody) g snakeTail

instance GridBuilder Goodie where
  gridBuilder (Goodie pos) grid = grid
    # Grid.insert pos Tile_Goodie
    # fromMaybe grid

--------------------------------------------------------------------------------
