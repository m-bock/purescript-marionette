module Test.Examples.Snake.Core
  ( Board(..)
  , Direction(..)
  , Tile(..)
  , Vec
  , Vec2(..)
  , parseBoard
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Array ((:))
import Data.Array as Arr
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as Str
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))

--- Vec2

data Vec2 a = Vec2 a a

instance Semiring a => Semiring (Vec2 a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance Ring a => Ring (Vec2 a) where
  sub = lift2 sub

derive instance Eq a => Eq (Vec2 a)
derive instance Ord a => Ord (Vec2 a)

derive instance Functor Vec2

derive instance Generic (Vec2 a) _

instance Show a => Show (Vec2 a) where
  show = genericShow

instance Applicative Vec2 where
  pure x = Vec2 x x

instance Apply Vec2 where
  apply (Vec2 f g) (Vec2 x y) = Vec2 (f x) (g y)

one_y :: forall a. Semiring a => Vec2 a
one_y = Vec2 zero one

one_x :: forall a. Semiring a => Vec2 a
one_x = Vec2 one zero

--- Vec 

type Vec = Vec2 Int

--- Direction

data Direction = Up | Right | Down | Left

directions :: Array Direction
directions = [ Up, Right, Down, Left ]

dirToVec :: Direction -> Vec
dirToVec = case _ of
  Left -> zero - one_x
  Right -> zero + one_x
  Up -> zero - one_y
  Down -> zero + one_y

--- Tile

data Tile
  = Tile_SnakeHead
  | Tile_SnakeBody
  | Tile_Goodie
  | Tile_Wall

derive instance Generic Tile _

instance Show Tile where
  show = genericShow

--

newtype Board = Board (Map Vec Tile)

derive instance Newtype Board _

data Snake = Snake Vec (Array Vec)

newtype Maze = Maze (Map Vec MazeItem)

data MazeItem = Wall

newtype Goodie = Goodie Vec

---

type Size = Vec

type Game =
  { snake :: Snake
  , board :: Board
  , size :: Size
  , goodies :: Array Vec
  , direction :: Direction
  }

type Env m =
  { delay :: Int -> m Unit
  , random :: m Int
  }

-- getFreeBoards :: Size -> Board -> Array Vec
-- getFreeBoards size = positionsInSize size <#> Arr.filter (\x -> )

-- initGame :: forall m. Env m -> m Game
-- initGame env = unsafeCoerce 1
--   where
--     size = Vec 10 10
--     initGoodie xs = do
--       x <-  

positionsInSize :: Vec -> Array Vec
positionsInSize (Vec2 sizex sizey) = do
  x <- Arr.range 0 (sizex - 1)
  y <- Arr.range 0 (sizey - 1)
  pure $ Vec2 x y

mkBoard :: Maze -> Snake -> Goodie -> Board
mkBoard (Maze maze) (Snake snakeHead snakeTail) (Goodie goodie) =
  Board $ Map.unions
    [ maze <#> case _ of
        Wall -> Tile_Wall
    , snakeTail
        <#> (\pos -> pos /\ Tile_SnakeBody)
        # Map.fromFoldable
    , Map.singleton snakeHead Tile_SnakeHead
    , Map.singleton goodie Tile_Goodie
    ]

boardToMaze :: Board -> Maze
boardToMaze = un Board
  >>> Map.mapMaybe case _ of
    Tile_Wall -> Just $ Wall
    _ -> Nothing
  >>> Maze

boardToSnake :: Board -> Maybe Snake
boardToSnake (Board board) = do
  head <- snakeHead
  let tail = snakeTail head []
  pure $ Snake head tail
  where

  snakeHead :: Maybe Vec
  snakeHead = board # Map.toUnfoldable # Arr.findMap \(Tuple k v) ->
    case v of
      Tile_SnakeHead -> Just k
      _ -> Nothing

  snakeTail :: Vec -> Array Vec -> Array Vec 
  snakeTail vec xs = case discoverDir vec of
    Just dir ->
      let
        vec' = dirToVec dir + vec
      in
        snakeTail vec' (vec' : xs)
    Nothing -> xs

  discoverDir :: Vec -> Maybe Direction
  discoverDir vec = directions # Arr.findMap \dir ->
    let
      vecCandidate = dirToVec dir + vec
    in
      case Map.lookup vecCandidate board of
        Just Tile_SnakeBody -> Just dir
        _ -> Nothing

-- discoverSnake :: Board -> Vec -> Maybe (Vec /\ Direction)
-- discoverSnake board vec = dirs # Arr.findMap \dir ->
--   let
--     vecCandidate = dirToVec dir + vec
--   in
--     case Map.lookup vecCandidate board of
--       Just Tile_SnakeBodyElem -> Just (vecCandidate /\ dir)
--       _ -> Nothing

parseChar :: Char -> Maybe Tile
parseChar = case _ of
  '#' -> Just Tile_Wall
  'O' -> Just Tile_SnakeBody
  '+' -> Just Tile_SnakeHead
  _ -> Nothing

parseBoard :: String -> Board
parseBoard = parseCharBoard
  >>> Map.mapMaybe parseChar
  >>> Board

parseCharBoard :: String -> Map Vec Char
parseCharBoard = trimNewlines
  >>> Str.split (Pattern "\n")
  >>> go
  >>> Map.fromFoldable
  where
  go lines =
    do
      y /\ line <- Arr.mapWithIndex Tuple lines
      x /\ char <- toCharArray line # Arr.mapWithIndex Tuple
      pure (Vec2 x y /\ char)

trimNewlines :: String -> String
trimNewlines =
  Regex.replace leadingNewlines "" >>> Regex.replace trailingNewlines ""
  where
  leadingNewlines = unsafeRegex "^\\n*" noFlags
  trailingNewlines = unsafeRegex "\\n*$" noFlags

printTile :: Maybe Tile -> Char
printTile = case _ of
  Nothing -> ' '
  Just Tile_Wall -> '#'
  Just Tile_SnakeBody -> 'O'
  Just Tile_SnakeHead -> '+'
  Just Tile_Goodie -> 'x'

printBoard :: Size -> Board -> String
printBoard (Vec2 sizex sizey) (Board board) =
  Arr.range 0 (sizey - 1)
    <#> mkLine
    # Str.joinWith "\n"

  where
  mkLine y =
    Arr.range 0 (sizex - 1)
      <#> (\x -> printTile $ Map.lookup (Vec2 x y) board)
      # fromCharArray

