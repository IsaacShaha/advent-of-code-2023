module PointOfIncidence2 where

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

mirrorHorizontal :: [[Char]] -> Maybe Int -> Maybe Int
mirrorHorizontal terrain bannedMirror
  = go bannedMirror [head terrain] (tail terrain)
  where
    go :: Maybe Int -> [[Char]] -> [[Char]] -> Maybe Int
    go _ _ [] = Nothing
    go bannedMirror topTerrain bottomTerrain
      | Just (length topTerrain) == bannedMirror
        = go bannedMirror (head bottomTerrain : topTerrain) (tail bottomTerrain)
      | take minTerrainLength topTerrain == take minTerrainLength bottomTerrain
        = Just (length topTerrain)
      | otherwise
        = go bannedMirror (head bottomTerrain : topTerrain) (tail bottomTerrain)
      where
        minTerrainLength = min (length topTerrain) (length bottomTerrain)

smudge :: Char -> Char
smudge '.' = '#'
smudge '#' = '.'
smudge c   = error $ "Unknown terrain " ++ show c ++ "."

smudge1D :: [Char] -> Int -> [Char]
smudge1D terrain x
  = take x terrain ++ [smudge (terrain!!x)] ++ drop (x + 1) terrain

smudge2D :: [[Char]] -> Int -> Int -> [[Char]]
smudge2D terrain x y
  = take y terrain ++ [smudge1D (terrain!!y) x] ++ drop (y + 1) terrain

smudgedMirrorHorizontal :: [[Char]] -> Maybe Int
smudgedMirrorHorizontal terrain
  = DM.listToMaybe
  . DM.catMaybes
  $ smudgedMirrors
  where
    bannedMirror = mirrorHorizontal terrain Nothing
    smudgedTerrain = smudges terrain
    smudgedMirrors = fmap (flip mirrorHorizontal bannedMirror) smudgedTerrain

smudgedMirrorVertical :: [[Char]] -> Maybe Int
smudgedMirrorVertical = smudgedMirrorHorizontal . DL.transpose

smudges :: [[Char]] -> [[[Char]]]
smudges terrain = go terrain 0 0
  where
    go :: [[Char]] -> Int -> Int -> [[[Char]]]
    go terrain x y
      | x == length (head terrain) = go terrain 0 (y + 1)
      | y == length terrain = []
      | otherwise = smudge2D terrain x y : go terrain (x + 1) y

main :: IO ()
main
    = print
    . CA.liftA2
        (+)
        (sum . DM.catMaybes . fmap smudgedMirrorVertical)
        ((*100) . sum . DM.catMaybes . fmap smudgedMirrorHorizontal)
  =<< U.unsafeParseFromFile fileParser "point-of-incidence.txt"
