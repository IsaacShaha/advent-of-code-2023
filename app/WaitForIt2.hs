module WaitForIt2 where

import qualified Control.Applicative as CA
import qualified Text.Parsec         as TP
import qualified Text.Parsec.String  as TPS
import qualified Utils               as U

-- Input Parsing

intParser :: TPS.Parser Int
intParser
    = read
    . concat
  <$> TP.sepEndBy1 (TP.many1 TP.digit) TP.spaces

fileParser :: TPS.Parser (Int, Int)
fileParser
  = (\[time,distance] -> (time,distance)) <$> TP.count 2 lineParser

lineParser = TP.manyTill TP.anyChar (TP.char ':') *> TP.spaces *> intParser

-- Logic

firstRecordBeater :: Int -> Int -> Int
firstRecordBeater totalTime recordDistance = firstRecordBeater' 0 totalTime
  where
    firstRecordBeater' :: Int -> Int -> Int
    firstRecordBeater' low high
      | distance > recordDistance && previousDistance < recordDistance
          = timeHeld
      | distance > recordDistance = firstRecordBeater' low (timeHeld - 1)
      | otherwise = firstRecordBeater' (timeHeld + 1) high
      where timeHeld = (low + high) `div` 2
            distance = generateDistance timeHeld totalTime
            previousDistance = generateDistance (timeHeld - 1) totalTime

generateDistance :: Int -> Int -> Int
generateDistance timeHeld totalTime = timeHeld * (totalTime - timeHeld)

lastRecordBeater :: Int -> Int -> Int
lastRecordBeater totalTime recordDistance = lastRecordBeater' 0 totalTime
  where
    lastRecordBeater' :: Int -> Int -> Int
    lastRecordBeater' low high
      | distance > recordDistance && nextDistance < recordDistance = timeHeld
      | distance > recordDistance = lastRecordBeater' (timeHeld + 1) high
      | otherwise = lastRecordBeater' low (timeHeld - 1)
      where timeHeld = (low + high) `div` 2
            distance = generateDistance timeHeld totalTime
            nextDistance = generateDistance (timeHeld + 1) totalTime

main :: IO ()
main
    = print
    . (+1)
    . CA.liftA2
        (subtract)
        (uncurry firstRecordBeater)
        (uncurry lastRecordBeater)
  =<< U.unsafeParseFromFile fileParser "input/wait-for-it.txt"
