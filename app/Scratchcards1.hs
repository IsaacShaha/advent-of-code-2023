module Scratchcards1 where

import qualified Data.Either        as DE
import qualified Text.Parsec        as TP
import qualified Text.Parsec.String as TPS
import qualified Utils              as U

data Card = Card {
  winningNumbers :: [Int]
, scratchNumbers :: [Int]
} deriving Show

cardParser :: TPS.Parser Card
cardParser = do
  TP.string "Card "
    *> TP.spaces
    *> TP.many1 TP.digit
    *> TP.char ':'
    *> TP.spaces
  winningNumbers <- numbersParser
  TP.char '|' *> TP.spaces
  scratchNumbers <- numbersParser
  return $ Card winningNumbers scratchNumbers

fileParser :: TPS.Parser [Card]
fileParser = TP.sepBy cardParser TP.newline

numbersParser :: TPS.Parser [Int]
numbersParser
  = TP.sepEndBy (read <$> TP.many1 TP.digit) (TP.many1 $ TP.char ' ')

score :: Card -> Int
score (Card winningNumbers scratchNumbers)
  = (\x -> if x > 0 then 2^(x-1) else 0)
  . length
  . filter id
  . fmap (flip elem winningNumbers)
  $ scratchNumbers

main :: IO ()
main
    = print
    . sum
    . filter (> 0)
    . fmap score
  =<< U.unsafeParseFromFile fileParser "input/scratchcards.txt"
