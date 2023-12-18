module PointOfIncidence1 where

import qualified Control.Applicative as CA
import qualified Data.List           as DL
import qualified Data.Maybe          as DM
import qualified Text.Parsec         as TP
import qualified Utils               as U

-- Input Parsing

type Parser = TP.Parsec String ()

fileParser :: Parser [[[Char]]]
fileParser = TP.sepBy terrainParser TP.newline

rowParser :: Parser [Char]
rowParser = TP.many1 (TP.oneOf ".#")

terrainParser :: Parser [[Char]]
terrainParser = TP.sepEndBy rowParser TP.newline

-- Logic

mirrorLocationHorizontal :: [[Char]] -> Maybe Int
mirrorLocationHorizontal terrain = go [head terrain] (tail terrain)
  where
    go :: [[Char]] -> [[Char]] -> Maybe Int
    go _ [] = Nothing
    go topTerrain bottomTerrain
      | take minTerrainLength topTerrain == take minTerrainLength bottomTerrain
        = Just (length topTerrain)
      | otherwise = go (head bottomTerrain : topTerrain) (tail bottomTerrain)
      where
        minTerrainLength = min (length topTerrain) (length bottomTerrain)

mirrorLocationVertical :: [[Char]] -> Maybe Int
mirrorLocationVertical = mirrorLocationHorizontal . DL.transpose

main :: IO ()
main
    = print
    . CA.liftA2
        (+)
        (sum . DM.catMaybes . fmap mirrorLocationVertical)
        ((*100) . sum . DM.catMaybes . fmap mirrorLocationHorizontal)
  =<< U.unsafeParseFromFile fileParser "point-of-incidence.txt"
