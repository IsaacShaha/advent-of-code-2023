{-# LANGUAGE OverloadedLists #-}

module PipeMaze2 where

import           Control.Applicative (empty, liftA2)
import           Data.Foldable       (maximumBy)
import qualified Data.List           as List (filter, zip)
import           Data.Maybe          (fromJust, fromMaybe)
import           Data.Sequence       (Seq (..), take, zip3)
import qualified Data.Sequence       as Sequence (filter, zip)
import           Data.Vector         (Vector, findIndex, (!), (!?))
import           GHC.Exts            (fromList)
import           Prelude             hiding (Left, Right, take, zip, zip3)
import           Text.Parsec         (char, choice, many1, newline, sepBy1)
import           Text.Parsec.String  (Parser)
import           Utils               (third3, unsafeParseFromFile)

import           Data.Foldable

-- Input Parsing

fileParser :: Parser (Vector (Vector Tile))
fileParser = fromList <$> sepBy1 lineParser newline

lineParser :: Parser (Vector Tile)
lineParser = fromList <$> many1 tileParser

tileParser :: Parser Tile
tileParser = choice [
      char '|' *> pure (Pipe Vertical)
    , char '-' *> pure (Pipe Horizontal)
    , char 'L' *> pure (Pipe NorthEastBend)
    , char 'J' *> pure (Pipe NorthWestBend)
    , char '7' *> pure (Pipe SouthWestBend)
    , char 'F' *> pure (Pipe SouthEastBend)
    , char '.' *> pure Ground
    , char 'S' *> pure Animal
  ]

-- Logic

sequenceConcat :: Seq (Seq a) -> Seq a
sequenceConcat (xs :<| xss) = xs <> sequenceConcat xss
sequenceConcat _            = empty

sequenceDropLast :: Seq a -> Seq a
sequenceDropLast (xs :|> _) = xs
sequenceDropLast _          = empty

sequenceNub :: Ord a => Seq a -> Seq a
sequenceNub (x :<| xs) = x :<| sequenceNub (Sequence.filter (/= x) xs)
sequenceNub _          = empty

sequenceTail :: Seq a -> Seq a
sequenceTail (_ :<| xs) = xs
sequenceTail _          = empty

type Coordinate = (Int, Int)

data Pipe
  = Vertical
  | Horizontal
  | NorthEastBend
  | NorthWestBend
  | SouthWestBend
  | SouthEastBend deriving (Eq, Show)

data Direction = North | East | South | West deriving (Bounded, Enum, Eq, Show)

captureUntil :: Vector (Vector Tile)
             -> (Coordinate -> Bool)
             -> Coordinate
             -> Direction
             -> Seq Coordinate
captureUntil tiles predicate coordinate direction
  | predicate coordinate = empty
  | otherwise
      = coordinate
    :<| (captureUntil tiles predicate =<< (nextCoordinate coordinate)) direction

directionsFromPipe :: Pipe -> [Direction]
directionsFromPipe Vertical      = [North, South]
directionsFromPipe Horizontal    = [East, West]
directionsFromPipe NorthEastBend = [North, East]
directionsFromPipe NorthWestBend = [North, West]
directionsFromPipe SouthWestBend = [South, West]
directionsFromPipe SouthEastBend = [South, East]

directionTo :: Coordinate -> Coordinate -> Direction
directionTo (x1, y1) (x2, y2)
  | x2 - x1 ==  0 && y2 - y1 == -1 = North
  | x2 - x1 ==  1 && y2 - y1 ==  0 = East
  | x2 - x1 ==  0 && y2 - y1 ==  1 = South
  | x2 - x1 == -1 && y2 - y1 ==  0 = West
  | otherwise                      = error "Not a valid direction."

isConnected :: Vector (Vector Tile) -> Coordinate -> Direction -> Bool
isConnected tiles coordinate
  = liftA2
      elem
      opposite
  $ directionsFromTile
  . tileFromVectors tiles
  . nextCoordinate coordinate

nextCoordinate :: Coordinate -> Direction -> Coordinate
nextCoordinate (x, y) North = (x, y - 1)
nextCoordinate (x, y) East  = (x + 1, y)
nextCoordinate (x, y) South = (x, y + 1)
nextCoordinate (x, y) West  = (x - 1, y)

opposite :: Direction -> Direction
opposite North = South
opposite East  = West
opposite South = North
opposite West  = East

pathDirections :: Seq Coordinate -> Seq Direction
pathDirections (coordinate1 :<| coordinate2 :<| coordinates)
    = directionTo coordinate1 coordinate2
  :<| pathDirections (coordinate2 :<| coordinates)
pathDirections _ = empty

data Tile = Pipe Pipe | Ground | Animal deriving (Eq, Show)

animalStart :: Vector (Vector Tile) -> Coordinate
animalStart tiles
  = ((,) =<< fromJust . findIndex (== Animal) . (tiles !))
  . fromJust
  . findIndex (elem Animal)
  $ tiles

connections :: Vector (Vector Tile) -> Coordinate -> [Direction]
connections tiles coordinate@(x, y)
    = fmap fst
    . List.filter snd
    . (flip List.zip =<< (fmap . isConnected tiles $ coordinate))
    . directionsFromTile . tileFromVectors tiles
    $ coordinate

directionsFromTile :: Tile -> [Direction]
directionsFromTile (Pipe pipe) = directionsFromPipe pipe
directionsFromTile Animal      = [minBound .. maxBound]
directionsFromTile _           = []

findLoop :: Vector (Vector Tile) -> Coordinate -> Seq Coordinate
findLoop tiles coordinate = go tiles coordinate [coordinate]
  where
    go :: Vector (Vector Tile) -> Coordinate -> Seq Coordinate -> Seq Coordinate
    go tiles start path@(_ :|> current)
      | length path == 1 = getLongestPath . getNextPaths $ nextTiles
      | elem start nextTiles && length path > 2 = path :|> start
      | otherwise = getLongestPath . getNextPaths . removeVisited $ nextTiles
      where
        getLongestPath
          = maximumBy (\path1 path2 -> length path1 `compare` length path2)
        getNextPaths = fmap (\nextTile -> go tiles start (path :|> nextTile))
        nextTiles = nextCoordinate current <$> connections tiles current
        removeVisited = List.filter (not . (`elem` path))

-- All tiles out of range are considered as ground.
tileFromVectors :: Vector (Vector Tile) -> Coordinate -> Tile
tileFromVectors tiles (x, y)
  = fromMaybe Ground
  . (!? x)
  . fromMaybe empty
  . (!? y)
  $ tiles

data Turn = Left | Straight | Right deriving (Eq, Show)

countTurns :: Seq Turn -> Turn -> Int
countTurns turns turn = length . Sequence.filter (== turn) $ turns

dominantTurn :: Seq Turn -> Turn
dominantTurn turns
  | countTurns turns Left > countTurns turns Right = Left
  | countTurns turns Left < countTurns turns Right = Right
  | otherwise                                      = Straight

pathTurns :: Seq Direction -> Seq Turn
pathTurns (direction1 :<| direction2 :<| directions)
    = turnTo direction1 direction2
  :<| pathTurns (direction2 :<| directions)
pathTurns _ = empty

turnFrom :: Direction -> Turn -> Direction
turnFrom North Left     = West
turnFrom North Straight = North
turnFrom North Right    = East
turnFrom East Left      = North
turnFrom East Straight  = East
turnFrom East Right     = South
turnFrom South Left     = East
turnFrom South Straight = South
turnFrom South Right    = West
turnFrom West Left      = South
turnFrom West Straight  = West
turnFrom West Right     = North

turnTo :: Direction -> Direction -> Turn
turnTo North West  = Left
turnTo North North = Straight
turnTo North East  = Right
turnTo East  North = Left
turnTo East  East  = Straight
turnTo East  South = Right
turnTo South East  = Left
turnTo South South = Straight
turnTo South West  = Right
turnTo West  South = Left
turnTo West  West  = Straight
turnTo West  North = Right
turnTo _ _         = error "Not a valid turn."

main :: IO ()
main = do
  tiles <- unsafeParseFromFile fileParser "pipe-maze.txt"
  let directions = pathDirections path
      dominantTurn' = dominantTurn turns
      path = findLoop tiles . animalStart $ tiles
      turns = pathTurns directions
  print
    . length
    . sequenceNub
    . sequenceConcat
    . fmap sequenceConcat
    . fmap
        ( \(coordinate, directions)
           -> fmap (uncurry $ captureUntil tiles (`elem` path))
            . (Sequence.zip =<< fmap (nextCoordinate coordinate))
            $ directions )
    . fmap
        ( \(coordinate, direction, turn)
           -> ( coordinate
              , case turn of
                  Straight -> [turnFrom direction dominantTurn']
                  _        -> [turnFrom direction dominantTurn', direction]
            :: Seq Direction ) )
    . Sequence.filter ((/= dominantTurn') . third3)
    . zip3
        (sequenceDropLast . sequenceTail $ path)
        (sequenceDropLast directions)
    $ turns
