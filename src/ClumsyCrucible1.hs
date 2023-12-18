module ClumsyCrucible1 where

import qualified Control.Monad as CM
import qualified Data.Char     as DC
import           Data.Vector   (Vector, (!), (!?))
import qualified Data.Vector   as DV
import qualified Text.Parsec   as TP
import qualified Utils         as U

import           Debug.Trace

-- | Input Parsing

type Parser = TP.Parsec String ()

fileParser :: Parser (Vector (Vector Int))
fileParser = DV.fromList <$> TP.sepBy1 lineParser TP.newline

intParser :: Parser Int
intParser = DC.digitToInt <$> TP.digit

lineParser :: Parser (Vector Int)
lineParser = DV.fromList <$> TP.many1 intParser

-- | Logic

dropLast :: [a] -> [a]
dropLast []     = []
dropLast [_]    = []
dropLast (x:xs) = x : dropLast xs

type Grid = Vector (Vector Int)

data Position = Position
  { row    :: Int
  , column :: Int
  } deriving (Eq, Show)

availablePositions :: Grid -> [Position] -> [Position]
availablePositions grid ((Position r c):previousPositions)
  = filter (\(Position r c) -> (grid !? r >>= (!? c)) /= Nothing)
  . filter (not . (`elem` previousPositions))
  $ [ Position (r - 1) c
    , Position (r + 1) c
    , Position r (c - 1)
    , Position r (c + 1) ]

heatLossAtPosition :: Grid -> Position -> Int
heatLossAtPosition grid (Position r c) = grid ! r ! c

-- TODO: Keep track of the lowest heat loss path calculated so far and stop
-- early on paths that cross that threshold.

lowestHeatLoss :: Grid -> Int
lowestHeatLoss grid = go [Position 0 0]
  where
    go :: [Position] -> Int
    go log@(currentPosition:previousPositions)
      | currentPosition ==
          Position (DV.length grid - 1) (DV.length (DV.head grid) - 1)
        = currentHeatLoss
      | otherwise
        = (currentHeatLoss +) . minimum . (0:) . fmap go
        . CM.ap (fmap (:) $ availablePositions grid log) . pure $ log
      where
        currentHeatLoss = heatLossAtPosition grid currentPosition

main :: IO ()
main = do
  input <- U.unsafeParseFromFile fileParser "clumsy-crucible.txt"
  print . lowestHeatLoss $ input
