module CubeConundrum where

import qualified Data.Either        as DE
import qualified Data.Foldable      as DF
import qualified Data.Map           as DM
import qualified Text.Parsec        as TP
import qualified Text.Parsec.String as TPS

type Hand = DM.Map String Int
defaultHand = DM.fromList [("red", 0), ("green", 0), ("blue", 0)]
maxHand = DM.fromList [("red", 12), ("green", 13), ("blue", 14)]

data Game = Game {
  gameID :: Int
, hands  :: [Hand]
}  deriving Show

colourParser :: TPS.Parser (String, Int)
colourParser = do
  amount <- read <$> TP.many1 TP.digit
  TP.char ' '
  colour <- TP.string "red" TP.<|> TP.string "green" TP.<|> TP.string "blue"
  return (colour, amount)

fileParser :: TPS.Parser [Game]
fileParser = TP.sepBy gameParser TP.newline

gameParser :: TPS.Parser Game
gameParser = do
  TP.string "Game "
  gameNumber <- read <$> TP.many1 TP.digit
  TP.string ": "
  hands <- TP.sepBy handParser (TP.string "; ")
  return . Game gameNumber $ hands

handParser :: TPS.Parser Hand
handParser
    = DF.foldl'
        (\hand (colour, amount) -> DM.insert colour amount hand)
        defaultHand
  <$> TP.sepBy colourParser (TP.string ", ")

minHand :: [Hand] -> Hand
minHand = DM.unionsWith max

power :: Hand -> Int
power = product . DM.elems

main :: IO ()
main
    = print
    . sum
    . map (power . minHand . hands)
    . DE.fromRight undefined
  =<< TPS.parseFromFile fileParser "input/cube-conundrum.txt"
