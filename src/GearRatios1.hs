module GearRatios where

import qualified Control.Applicative as CA
import qualified Data.Char           as DC
import qualified Data.List           as DL
import qualified Data.Set            as DS
import qualified System.IO           as SI

index2D :: [[a]] -> [[((Int, Int), a)]]
index2D = zipWith (zip . zip [0..] . repeat) [0..]

partNumbers :: DS.Set (Int, Int) -> [[((Int, Int), Char)]] -> [Int]
partNumbers ss cs
    = concat
    . DL.foldl'
      ( \accumulator c
         -> (:accumulator)
          . fmap (read . snd)
          . filter
            ( (\((x1, x2), y) -> searchSymbol ss [x1-1..x2+1] [y-1..y+1])
            . fst )
          . fmap sparse
          . filter (DC.isDigit . snd . head)
          . DL.groupBy
            (\c1 c2 -> (DC.isDigit . snd $ c1) == (DC.isDigit . snd $ c2))
          $ c )
      []
    $ cs

searchSymbol :: DS.Set (Int, Int) -> [Int] -> [Int] -> Bool
searchSymbol ss xs = any id . map (flip DS.member ss) . CA.liftA2 (,) xs

sparse :: [((Int, Int), a)] -> (((Int, Int), Int), [a])
sparse
    = CA.liftA2
      (,)
      ( CA.liftA2
        (,)
        (CA.liftA2 (,) (head) (last) . fmap fst)
        (snd . head)
      . fmap fst )
    $ fmap snd

symbolSet :: [[((Int, Int), Char)]] -> DS.Set (Int, Int)
symbolSet
  = DS.fromList
  . fmap fst
  . filter (CA.liftA2 (&&) (not . DC.isDigit) (/='.') . snd)
  . concat

main :: IO ()
main
    = print
    . sum
    . (partNumbers =<< symbolSet)
    . index2D
    . lines
  =<< SI.hGetContents
  =<< SI.openFile "input/gear-ratios.txt" SI.ReadMode
