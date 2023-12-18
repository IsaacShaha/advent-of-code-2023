module CosmicExpansion1 where

import qualified Data.List   as DL
import qualified Text.Parsec as TP
import           Text.Parsec ((<|>))
import qualified Utils       as U

-- Logic

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distances :: [(Int, Int)] -> [Int]
distances (galaxy:nextGalaxies)
  = fmap (distance galaxy) nextGalaxies ++ distances nextGalaxies
distances [] = []

expandv :: [[Char]] -> [[Char]]
expandv (row1 : nextRows)
  | all (=='.') row1 = row1 : row1 : expandv nextRows
  | otherwise = row1 : expandv nextRows
expandv [] = []

expand :: [[Char]] -> [[Char]]
expand = DL.transpose . expandv . DL.transpose . expandv

galaxies :: [[Char]] -> [(Int, Int)]
galaxies image
  = concat
  . zipWith (\y xs -> zip xs (repeat y)) [0..]
  . fmap (DL.elemIndices '#')
  $ image

-- Input Parsing

type Parser = TP.Parsec String ()

fileParser :: Parser [[Char]]
fileParser = TP.sepBy lineParser TP.newline

lineParser :: Parser [Char]
lineParser = TP.many1 (TP.char '.' <|> TP.char '#')

main :: IO ()
main
    = print
    . sum
    . distances
    . galaxies
    . expand
  =<< U.unsafeParseFromFile fileParser "cosmic-expansion.txt"
