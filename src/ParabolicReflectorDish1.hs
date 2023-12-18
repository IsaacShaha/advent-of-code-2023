module ParabolicReflectorDish1 where

import qualified Data.List   as DL
import qualified Text.Parsec as TP
import qualified Utils       as U

-- Input Parsing

type Parser = TP.Parsec String ()

fileParser :: Parser [[Tile]]
fileParser = TP.sepBy rowParser TP.newline

rowParser :: Parser [Tile]
rowParser = TP.many1 $ TP.choice [
    TP.char '#' *> pure CubeRock
  , TP.char 'O' *> pure RoundRock
  , TP.char '.' *> pure Empty
  ]

data Tile = CubeRock | RoundRock | Empty deriving (Eq, Show)

-- Logic

northLoad :: [[Tile]] -> Int
northLoad []            = 0
northLoad rows@(row:nextRows)
  = (length . filter (== RoundRock) $ row) * length rows + northLoad nextRows

tiltEast :: [[Tile]] -> [[Tile]]
tiltEast = fmap go
  where
    go :: [Tile] -> [Tile]
    go (RoundRock : Empty : nextTiles) = Empty : go (RoundRock : nextTiles)
    go (RoundRock : RoundRock : nextTiles)
      | head subSolution == RoundRock = RoundRock : go (RoundRock : nextTiles)
      | otherwise = go (RoundRock : subSolution)
      where
        subSolution = go $ RoundRock : nextTiles
    go (tile : nextTiles)              = tile : go nextTiles
    go []                              = []

tiltNorth :: [[Tile]] -> [[Tile]]
tiltNorth = reverse . DL.transpose . tiltEast . DL.transpose . reverse

main :: IO ()
main
    = print
    . northLoad
    . tiltNorth
  =<< U.unsafeParseFromFile fileParser "parabolic-reflector-dish.txt"
