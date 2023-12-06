module GearRatios where

import qualified Control.Applicative as CA
import qualified Data.Char           as DC
import qualified Data.List           as DL
import qualified Data.Map            as DMp
import qualified Data.Maybe          as DMy
import qualified System.IO           as SI

gearRatios :: [(Int, Int)] -> DMp.Map (Int, Int) ([Int]) -> [Int]
gearRatios gs nns
    = fmap product
    . filter ((==2) . length)
    . DMy.catMaybes
    . fmap (flip DMp.lookup nns)
    $ gs

gears :: [[((Int, Int), Char)]] -> [(Int, Int)]
gears = fmap fst . filter ((=='*') . snd) . concat

index2D :: [[a]] -> [[((Int, Int), a)]]
index2D = zipWith (zip . zip [0..] . repeat) [0..]

numberNeighboursSet :: [[((Int, Int), Char)]] -> DMp.Map (Int, Int) ([Int])
numberNeighboursSet cs
    = DL.foldl'
      ( \accumulator (((x1, x2), y), n)
         -> DMp.unionWith (++) accumulator
          $ DMp.fromList [((x, y), [n])|x<-[x1-1..x2+1],y <-[y-1..y+1]] )
      DMp.empty
    . concat
    . DL.foldl'
      ( \accumulator char
         -> (:accumulator)
          . fmap (CA.liftA2 (,) (id . fst) (read . snd) . sparse)
          . filter (DC.isDigit . snd . head)
          . DL.groupBy
            (\char1 char2
             -> (DC.isDigit . snd $ char1) == (DC.isDigit . snd $ char2))
          $ char )
      []
    $ cs

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

main :: IO ()
main
    = print
    . sum
    . CA.liftA2 gearRatios gears numberNeighboursSet
    . index2D
    . lines
  =<< SI.hGetContents
  =<< SI.openFile "input/gear-ratios.txt" SI.ReadMode
