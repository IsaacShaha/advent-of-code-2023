module CosmicExpansion2 where

import qualified Data.List   as DL
import qualified Text.Parsec as TP
import           Text.Parsec ((<|>))
import qualified Utils       as U

-- Logic

data Space = Galaxy | Space Int deriving (Eq, Show)

allDistances :: ([Int], [Int]) -> [(Int, Int)] -> [Int]
allDistances _ [] = []
allDistances distances (galaxy:nextGalaxies)
    = fmap (distance2D distances galaxy) nextGalaxies
   ++ allDistances distances nextGalaxies

distance1D :: [Int] -> Int -> Int -> Int
distance1D distances x1 x2
  | x1 > x2 = distance1D distances x2 x1
  | otherwise = sum . take (x2 - x1) . drop (x1 + 1) $ distances

distance2D :: ([Int], [Int]) -> (Int, Int) -> (Int, Int) -> Int
distance2D (horizontalDistances, verticalDistances) (x1, y1) (x2, y2)
  = distance1D horizontalDistances x1 x2 + distance1D verticalDistances y1 y2

isSpace :: Space -> Bool
isSpace (Space _) = True
isSpace _         = False

galaxies :: [[Space]] -> [(Int, Int)]
galaxies image
  = concat
  . zipWith (\y xs -> zip xs (repeat y)) [0..]
  . fmap (DL.elemIndices Galaxy)
  $ image

segmentVerticalDistances :: [[Space]] -> [Int]
segmentVerticalDistances [] = []
segmentVerticalDistances (spaces:nextSpaces)
  | all isSpace spaces = 1000000 : segmentVerticalDistances nextSpaces
  | otherwise          = 1       : segmentVerticalDistances nextSpaces

-- Input Parsing

type Parser = TP.Parsec String ()

fileParser :: Parser [[Space]]
fileParser = TP.sepBy lineParser TP.newline

lineParser :: Parser [Space]
lineParser = TP.many1 spaceParser

spaceParser :: Parser Space
spaceParser = (TP.char '.' *> pure (Space 1)) <|> (TP.char '#' *> pure Galaxy)

main :: IO ()
main = do
  image <- U.unsafeParseFromFile fileParser "cosmic-expansion.txt"
  let galaxies' = galaxies image
      horizontalDistances = segmentVerticalDistances . DL.transpose $ image
      verticalDistances = segmentVerticalDistances image
  print
    . sum
    . allDistances (horizontalDistances, verticalDistances)
    $ galaxies'
