module Scratchcards where

import qualified Data.Either        as DE
import qualified Data.List          as DL
import qualified Data.Maybe         as DM
import qualified Data.Sequence      as DS
import qualified Text.Parsec        as TP
import qualified Text.Parsec.String as TPS

data Card = Card {
  cardNumber     :: Int
, winningNumbers :: [Int]
, scratchNumbers :: [Int]
} deriving Show

bulkUpdateSequence :: DS.Seq a -> [(Int, (a -> a))] -> DS.Seq a
bulkUpdateSequence
  = DL.foldl'
    (\accumulator (index, update) -> DS.adjust' update index accumulator)

cardParser :: TPS.Parser Card
cardParser = do
  cardNumber <- TP.string "Card " *> TP.spaces *> (read <$> (TP.many1 TP.digit))
  winningNumbers <- TP.char ':' *> TP.spaces *> numbersParser
  TP.char '|' *> TP.spaces
  scratchNumbers <- numbersParser
  return $ Card cardNumber winningNumbers scratchNumbers

cardsWon :: Card -> Int
cardsWon (Card cardNumber winningNumbers scratchNumbers)
  = length
  . filter id
  . fmap (flip elem winningNumbers)
  $ scratchNumbers

fileParser :: TPS.Parser [Card]
fileParser = TP.sepBy cardParser TP.newline

numbersParser :: TPS.Parser [Int]
numbersParser
  = TP.sepEndBy (read <$> TP.many1 TP.digit) (TP.many1 $ TP.char ' ')

numCards :: [Card] -> Int
numCards cards
  = sum
  . DL.foldl'
    ( \accumulator card ->
        let cardNumber' = cardNumber card
        in  bulkUpdateSequence accumulator
          . zip [cardNumber' .. cardNumber' + cardsWon card - 1]
          . repeat
          $ (+) (DM.fromJust $ DS.lookup (cardNumber' - 1) accumulator) )
    (DS.fromList $ replicate (length cards) 1)
  $ cards

main :: IO ()
main
    = print
    . numCards
    . DE.fromRight undefined
  =<< TPS.parseFromFile fileParser "input/scratchcards.txt"
