module HotSprings2 where

import qualified Data.Function.Memoize as DFM
import qualified Data.List             as DL
import qualified Text.Parsec           as TP
import qualified Utils                 as U

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

data Spring = Damaged | Operational | Unknown deriving (Bounded, Enum, Eq, Show)

instance DFM.Memoizable Spring where memoize = DFM.memoizeFinite

maxCurrentDamage :: [Spring] -> Int
maxCurrentDamage []                        = 0
maxCurrentDamage (Operational:nextSprings) = 0
maxCurrentDamage (spring:nextSprings)      = 1 + maxCurrentDamage nextSprings

maxCurrentDamage' = DFM.memoize maxCurrentDamage

possibilities :: [Spring] -> [Int] -> Int
possibilities springs counts = go' springs counts False
  where
    go :: [Spring] -> [Int] -> Bool -> Int
    go [] [] _ = 1
    go [] (_:_) _ = noMatch
    go (Damaged:_) _ True = noMatch
    go (_:nextSprings) counts True = go' nextSprings counts False
    go (Damaged:_) [] _ = noMatch
    go (_:nextSprings) [] _ = go' nextSprings [] False
    go springs@(Damaged:_) (count:nextCounts) _
      | maxCurrentDamage' springs >= count
        = go' (drop count springs) nextCounts True
      | otherwise = noMatch
    go (Unknown:nextSprings) counts _
      = go' (Damaged:nextSprings) counts False
      + go' (Operational:nextSprings) counts False
    go (_:nextSprings) counts _ = go' nextSprings counts False
    go' = DFM.memoize3 go
    noMatch = 0

main :: IO ()
main
    = print
    . sum
    . fmap
        ( uncurry possibilities
        . \(springs, counts)
          -> ( DL.intercalate [Unknown] . replicate 5 $ springs
            , concat . replicate 5 $ counts ) )
  =<< U.unsafeParseFromFile fileParser "hot-springs.txt"
