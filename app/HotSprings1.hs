module HotSprings1 where

import qualified Data.Maybe  as DM
import qualified Text.Parsec as TP
import qualified Utils       as U

import           Debug.Trace

-- Input Parsing

type Parser = TP.Parsec String ()

intParser :: Parser Int
intParser = read <$> TP.many1 TP.digit

fileParser :: Parser [([Spring], [Int])]
fileParser = TP.sepBy rowParser TP.newline

rowParser :: Parser ([Spring], [Int])
rowParser = do
  springs <- TP.many1 springParser
  TP.char ' '
  counts <- TP.sepBy intParser (TP.char ',')
  return (springs, counts)

springParser :: Parser Spring
springParser = TP.choice [
    TP.char '#' *> pure Damaged
  , TP.char '.' *> pure Operational
  , TP.char '?' *> pure Unknown
  ]

-- Logic

data Spring = Damaged | Operational | Unknown deriving (Eq, Show)

possibilities :: [Spring] -> [Int] -> [[Spring]]
possibilities springs counts = DM.catMaybes $ go springs counts False
  where
    go :: [Spring] -> [Int] -> Bool -> [Maybe [Spring]]
    go [] [] _ = [Just []]
    go [] (_:_) _ = noMatch
    go (Damaged:_) _ True = noMatch
    go (_:nextSprings) counts True
      = fmap (Operational :) <$> go nextSprings counts False
    go (Damaged:_) [] _ = noMatch
    go (_:nextSprings) [] _
      = fmap (Operational :) <$> go nextSprings [] False
    go springs@(Damaged:_) (count:nextCounts) _
      | (length . takeWhile (/= Operational) $ springs) >= count
        = fmap (replicate count Damaged ++)
      <$> go (drop count springs) nextCounts True
      | otherwise = noMatch
    go (Unknown:nextSprings) counts _
      = go (Damaged:nextSprings) counts False
     ++ go (Operational:nextSprings) counts False
    go (_:nextSprings) counts _
      = fmap (Operational :) <$> go nextSprings counts False
    noMatch = [Nothing]

main :: IO ()
main = do
  input <- U.unsafeParseFromFile fileParser "hot-springs.txt"
  print
    . sum
    . fmap (length . uncurry possibilities)
    $ input
