module Trebuchet1 where

import qualified Control.Applicative as CA
import qualified Data.Char           as DC
import qualified System.IO           as SI

calibrationValue :: String -> Int
calibrationValue = CA.liftA2 (+) ((10*) . head) last . ints

ints :: String -> [Int]
ints = fmap DC.digitToInt . filter (DC.isDigit)

main :: IO ()
main
    = print
    . sum
    . fmap calibrationValue
    . lines
  =<< SI.hGetContents
  =<< SI.openFile "input/trebutchet.txt" SI.ReadMode
