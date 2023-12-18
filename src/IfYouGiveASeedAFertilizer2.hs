module IfYouGiveASeedAFertilizer2 where

import qualified Control.Applicative as CA
import qualified Data.Either         as DE
import qualified Data.Foldable       as DF
import qualified Data.List           as DL
import qualified Data.Sequence       as DS
import qualified Text.Parsec         as TP
import qualified Text.Parsec.String  as TPS
import qualified Utils               as U

type Map = DS.Seq Mapping
data Mapping = Mapping {
  from  :: Int
, to    :: Int
, range :: Int
} deriving (Eq, Show)
instance Ord Mapping where
  compare (Mapping from1 _ _) (Mapping from2 _ _) = compare from1 from2

-- Input Parsing

fileParser :: TPS.Parser ([(Int, Int)], [Map])
fileParser = CA.liftA2 (,) seedParser mapsParser

intsParser :: TPS.Parser [Int]
intsParser = TP.sepBy1 (read <$> TP.many1 TP.digit) $ TP.char ' '

mapParser :: TPS.Parser Map
mapParser = DS.fromList <$> TP.sepEndBy mappingParser TP.newline

mappingParser :: TPS.Parser Mapping
mappingParser = fmap (\[to,from,range] -> Mapping from to range) intsParser

mapsParser :: TPS.Parser [Map]
mapsParser = TP.many1 $ TP.manyTill TP.anyToken (TP.string ":\n") *> mapParser

seedParser :: TPS.Parser [(Int, Int)]
seedParser
    = TP.string "seeds: "
   *> fmap (fmap (uncurry (,)) . pairs) intsParser

-- Logic

alignMaps :: Map -> Map -> (Map, Map)
alignMaps map1 map2
  = (,)
    (DS.sortBy (\(Mapping _ to1 _) (Mapping _ to2 _) -> compare to1 to2) map1)
  $ DS.sort map2

applyMapRange :: (Int, Int) -> Map -> [Int]
applyMapRange seeds DS.Empty = undefined
applyMapRange (seedFrom, seedRange) map
  = DF.toList
  . fmap to
  . (\(map' DS.:|> Mapping from to range)
         -> map'
     DS.:|> Mapping
              from
              (seedFrom + seedRange - 1)
              (seedFrom + seedRange - from) )
  . DS.takeWhileL ((seedFrom + seedRange - 1 >=) . from)
  . (\(Mapping from to range DS.:<| map') ->
      Mapping seedFrom to (from + range - seedFrom) DS.:<| map' )
  . DS.dropWhileL (not . fitsInMapping seedFrom)
  $ map

composeMaps :: Map -> Map -> Map
composeMaps map1 map2 = uncurry composeMaps' $ alignMaps map1 map2
  where
    composeMaps' :: Map -> Map -> Map
    composeMaps'
      map1@(mapping1@(Mapping from1 to1 range1) DS.:<| map1')
      map2@(mapping2@(Mapping from2 to2 range2) DS.:<| map2')
      | range1 == range2
                = Mapping from1 to2 range1
           DS.:<| composeMaps' map1' map2'
      | range1 < range2
                = Mapping from1 to2 range1
           DS.:<| composeMaps'
                          map1'
                          ( Mapping
                              (from2 + range1)
                              (to2 + range1)
                              (range2 - range1)
                     DS.:<| map2' )
      | otherwise
                = Mapping from1 to2 range2
           DS.:<| composeMaps'
                          ( Mapping
                              (from1 + range2)
                              (to1 + range2)
                              (range1 - range2)
                     DS.:<| map1' )
                          map2'
    composeMaps' DS.Empty map2 = map2
    composeMaps' map1 DS.Empty = map1

fillMap :: Map -> Map
fillMap map =
  let lowestFrom = case map of (Mapping from to range) DS.:<| _ -> from
  in  DL.foldl'
        ( \mappings mapping2@(Mapping from2 to2 range2) ->
            case mappings of
              _ DS.:|> mapping1@(Mapping from1 to1 range1) ->
                if        from2 > from1 + range1
                then      ( mappings
                   DS.:|> Mapping
                            (from1 + range1)
                            (from1 + range1)
                            (from2 - from1 - range1) )
                   DS.:|> mapping2
                else      mappings DS.:|> mapping2
              otherwise -> DS.singleton mapping2 )
        ( if    lowestFrom > 0
          then  DS.singleton $ Mapping 0 0 lowestFrom
          else  DS.empty )
    $ map

fitsInMapping :: Int -> Mapping -> Bool
fitsInMapping seed (Mapping from _ range) = seed >= from && seed < from + range

pairs :: [a] -> [(a, a)]
pairs []         = []
pairs (x1:x2:xs) = (x1, x2) : pairs xs

main :: IO ()
main
    = print
    . minimum
    . concat
    . CA.liftA2
      (<*>)
      (fmap applyMapRange . fst)
      ( pure
      . DS.sort
      . DL.foldl1' composeMaps
      . fmap (fillMap . DS.sort)
      . snd )
  =<< U.unsafeParseFromFile fileParser "if-you-give-a-seed-fertilizer.txt"
