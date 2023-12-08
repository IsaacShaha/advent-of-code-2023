module WaitForIt1 where

import qualified Control.Applicative as CA
import qualified Text.Parsec         as TP
import qualified Text.Parsec.String  as TPS
import qualified Utils               as U

-- Input Parsing

intParser :: TPS.Parser Int
intParser = read <$> TP.many1 TP.digit

intsParser :: TPS.Parser [Int]
intsParser = TP.sepEndBy1 intParser TP.spaces

fileParser :: TPS.Parser [(Int, Int)]
fileParser
  = CA.liftA2
    zip
    (TP.string "Time:" *> TP.spaces *> intsParser)
  $ TP.string "Distance:" *> TP.spaces *> intsParser

-- Logic

generateDistances :: Int -> [Int]
generateDistances time
  = fmap (uncurry (*))
  . zip [0..time]
  . reverse
  $ [0..time]

recordDistances :: Int -> Int -> [Int]
recordDistances time recordDistance
  = filter (> recordDistance) $ generateDistances time

main :: IO ()
main
    = print
    . product
    . map (length . uncurry recordDistances)
  =<< U.unsafeParseFromFile fileParser "input/wait-for-it.txt"
