module ParabolicReflectorDish2 where

import           Data.Function.Memoize (Memoizable)
import qualified Data.Function.Memoize as DFM
import qualified Data.List             as DL
import qualified Text.Parsec           as TP
import qualified Utils                 as U

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

data Tile = CubeRock | RoundRock | Empty deriving (Bounded, Enum, Eq, Show)

instance Memoizable Tile where memoize = DFM.memoizeFinite

-- Logic

northLoad :: [[Tile]] -> Int
northLoad []            = 0
northLoad rows@(row:nextRows)
  = (length . filter (== RoundRock) $ row) * length rows + northLoad nextRows

spinCycle :: [[Tile]] -> [[Tile]]
spinCycle = DFM.memoize spinCycle'
  where
    spinCycle' :: [[Tile]] -> [[Tile]]
    spinCycle' = tiltEast . tiltSouth . tiltWest . tiltNorth

spinCycles :: Int -> [[Tile]] -> [[Tile]]
spinCycles iterations platform
  = iterate spinCycle platform !! (cycleStart + remainingCycles)
  where
    (cycleStart, cycleEnd) = findCycle [platform]
    findCycle :: [[[Tile]]] -> (Int, Int)
    findCycle xs@(x:nextXs) = case DL.elemIndex x nextXs of
      Just index -> (length nextXs - 1 - index, length nextXs)
      Nothing    -> findCycle (spinCycle x : xs)
    remainingCycles = (iterations - cycleStart) `mod` (cycleEnd - cycleStart)

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

tiltSouth :: [[Tile]] -> [[Tile]]
tiltSouth = DL.transpose . tiltEast . DL.transpose

tiltWest :: [[Tile]] -> [[Tile]]
tiltWest = fmap reverse . tiltEast . fmap reverse

main :: IO ()
main
    = print
    . northLoad
    . spinCycles 1000000000
  =<< U.unsafeParseFromFile fileParser "parabolic-reflector-dish.txt"
