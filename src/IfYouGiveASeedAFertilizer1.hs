module IfYouGiveASeedAFertilizer where

import qualified Control.Applicative as CA
import qualified Control.Monad       as CM
import qualified Data.Either         as DE
import qualified Data.Foldable       as DF
import qualified Text.Parsec         as TP
import qualified Text.Parsec.String  as TPS

type Map = [Mapping]
data Mapping = Mapping {
  from  :: Int
, to    :: Int
, range :: Int
} deriving Show

-- Input Parsing

fileParser :: TPS.Parser ([Int], [Map])
fileParser = CM.liftM2 (,) seedParser mapsParser

intsParser :: TPS.Parser [Int]
intsParser = TP.sepBy1 (read <$> TP.many1 TP.digit) $ TP.char ' '

mapParser :: TPS.Parser Map
mapParser = TP.sepEndBy mappingParser TP.newline

mappingParser :: TPS.Parser Mapping
mappingParser = fmap (\[to,from,range] -> Mapping from to range) intsParser

mapsParser :: TPS.Parser [Map]
mapsParser = TP.many1 $ TP.manyTill TP.anyToken (TP.string ":\n") *> mapParser

seedParser :: TPS.Parser [Int]
seedParser = TP.string "seeds: " *> intsParser

-- Logic

applyMap :: Int -> Map -> Int
applyMap seed map
  = case DF.find (fitsInMapping seed) map of
      Just (Mapping from to range) -> to - from + seed
      Nothing                      -> seed

fitsInMapping :: Int -> Mapping -> Bool
fitsInMapping seed (Mapping from _ range) = seed >= from && seed < from + range

seedLocation :: Int -> [Map] -> Int
seedLocation seed maps = DF.foldl' applyMap seed maps

main :: IO ()
main
    = print
    . minimum
    . CA.liftA2 (<*>) (fmap seedLocation . fst) (pure . snd)
  =<< DE.fromRight undefined
  <$> TPS.parseFromFile fileParser "input/if-you-give-a-seed-fertilizer.txt"
