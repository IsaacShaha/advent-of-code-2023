module Trebuchet where

import qualified Control.Monad as CM (liftM2, mapM_)
import qualified Data.Char     as DC (digitToInt, isDigit)
import qualified Data.List     as DL (isPrefixOf)
import qualified System.IO     as SI (IOMode (ReadMode), hGetContents, openFile)

calibrationValue :: String -> Int
calibrationValue = CM.liftM2 (+) ((10*) . head) last . ints

digitToInt :: String -> Maybe Int
digitToInt xs
  | startsWith "zero" = Just 0
  | startsWith "one" = Just 1
  | startsWith "two" = Just 2
  | startsWith "three" = Just 3
  | startsWith "four" = Just 4
  | startsWith "five" = Just 5
  | startsWith "six" = Just 6
  | startsWith "seven" = Just 7
  | startsWith "eight" = Just 8
  | startsWith "nine" = Just 9
  | DC.isDigit . head $ xs = Just . DC.digitToInt . head $ xs
  | otherwise = Nothing
  where startsWith = flip DL.isPrefixOf xs

ints :: String -> [Int]
ints [] = []
ints xs = case digitToInt xs of
  Just x  -> x : ints (tail xs)
  Nothing -> ints (tail xs)

main :: IO ()
main = do
  input <- SI.hGetContents =<< SI.openFile "input/trebutchet.txt" SI.ReadMode
  let lines = words input
  print . sum . map calibrationValue $ lines

