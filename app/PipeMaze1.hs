{-# LANGUAGE OverloadedLists #-}

module PipeMaze1 where

import           Control.Applicative (liftA2)
import           Data.Foldable       (maximumBy)
import           Data.Maybe          (fromJust, fromMaybe)
import           Data.Sequence       (Seq (..))
import           Data.Vector         (Vector, empty, findIndex, (!), (!?))
import qualified Data.Vector         as Vector
import           Text.Parsec         (char, choice, many1, newline, sepBy1)
import           Text.Parsec.String  (Parser)
import           Utils               (unsafeParseFromFile)

-- Input Parsing

fileParser :: Parser (Vector (Vector Tile))
fileParser = Vector.fromList <$> sepBy1 lineParser newline

lineParser :: Parser (Vector Tile)
lineParser = Vector.fromList <$> many1 tileParser

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

type Key = (Int, Int)

data Pipe
  = Vertical
  | Horizontal
  | NorthEastBend
  | NorthWestBend
  | SouthWestBend
  | SouthEastBend deriving (Eq, Show)

data Direction = North | East | South | West deriving (Bounded, Enum, Eq, Show)

directionsFromPipe :: Pipe -> [Direction]
directionsFromPipe Vertical      = [North, South]
directionsFromPipe Horizontal    = [East, West]
directionsFromPipe NorthEastBend = [North, East]
directionsFromPipe NorthWestBend = [North, West]
directionsFromPipe SouthWestBend = [South, West]
directionsFromPipe SouthEastBend = [South, East]

isConnected :: Vector (Vector Tile) -> Key -> Direction -> Bool
isConnected tiles key
  = liftA2
      elem
      opposite
  $ directionsFromTile
  . tileFromVectors tiles
  . nextIndex key

nextIndex :: Key -> Direction -> Key
nextIndex (x, y) North = (x, y - 1)
nextIndex (x, y) East  = (x + 1, y)
nextIndex (x, y) South = (x, y + 1)
nextIndex (x, y) West  = (x - 1, y)

opposite :: Direction -> Direction
opposite North = South
opposite East  = West
opposite South = North
opposite West  = East

data Tile = Pipe Pipe | Ground | Animal deriving (Eq, Show)

animalStart :: Vector (Vector Tile) -> Key
animalStart tiles
  = ((,) =<< fromJust . findIndex (== Animal) . (tiles !))
  . fromJust
  . findIndex (elem Animal)
  $ tiles

connections :: Vector (Vector Tile) -> Key -> [Direction]
connections tiles key@(x, y)
    = fmap fst
    . filter snd
    . (flip zip =<< (fmap . isConnected tiles $ key))
    . directionsFromTile . tileFromVectors tiles
    $ key

directionsFromTile :: Tile -> [Direction]
directionsFromTile (Pipe pipe) = directionsFromPipe pipe
directionsFromTile Animal      = [minBound .. maxBound]
directionsFromTile _           = []

findLoop :: Vector (Vector Tile) -> Key -> Seq Key
findLoop tiles key = go tiles key [key]
  where
    go :: Vector (Vector Tile) -> Key -> Seq Key -> Seq Key
    go tiles start path@(_ :|> current)
      | length path == 1 = getLongestPath . getNextPaths $ nextTiles
      | elem start nextTiles && length path > 2 = path :|> start
      | otherwise = getLongestPath . getNextPaths . removeVisited $ nextTiles
      where
        getLongestPath
          = maximumBy (\path1 path2 -> length path1 `compare` length path2)
        getNextPaths = fmap (\nextTile -> go tiles start (path :|> nextTile))
        nextTiles = nextIndex current <$> connections tiles current
        removeVisited = filter (not . (`elem` path))

-- All tiles out of range are considered as ground.
tileFromVectors :: Vector (Vector Tile) -> Key -> Tile
tileFromVectors tiles (x, y)
  = fromMaybe Ground
  . (!? x)
  . fromMaybe empty
  . (!? y)
  $ tiles

main :: IO ()
main = do
    tiles <- unsafeParseFromFile fileParser "pipe-maze.txt"
    print . (`div` 2) . subtract 1 . length . findLoop tiles . animalStart $ tiles
