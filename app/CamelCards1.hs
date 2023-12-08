module CamelCards1 where

import qualified Control.Applicative as CA
import qualified Data.Char           as DC
import qualified Data.List           as DL
import qualified Text.Parsec         as TP
import qualified Text.Parsec.String  as TPS
import qualified Utils               as U

data Card = Card Char deriving (Eq, Show)

instance Enum Card where
    fromEnum (Card 'T') = 8
    fromEnum (Card 'J') = 9
    fromEnum (Card 'Q') = 10
    fromEnum (Card 'K') = 11
    fromEnum (Card 'A') = 12
    fromEnum (Card c)   = DC.digitToInt c - 2

    toEnum 8  = Card 'T'
    toEnum 9  = Card 'J'
    toEnum 10 = Card 'Q'
    toEnum 11 = Card 'K'
    toEnum 12 = Card 'A'
    toEnum i  = Card $ DC.intToDigit $ i + 2

instance Ord Card where
  compare card1 card2 = compare (fromEnum card1) (fromEnum card2)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind deriving (Eq, Ord, Show)

type Bid = Int
data Hand = Hand Card Card Card Card Card Bid deriving (Eq, Show)

instance Ord Hand where
  compare
    hand1@(Hand card1A card1B card1C card1D card1E _)
    hand2@(Hand card2A card2B card2C card2D card2E _)
    = compare
        (handType hand1, card1A, card1B, card1C, card1D, card1E)
        (handType hand2, card2A, card2B, card2C, card2D, card2E)

bid :: Hand -> Bid
bid (Hand _ _ _ _ _ bid) = bid

handType :: Hand -> HandType
handType hand
  | hasGroup 5 hand                     = FiveOfAKind
  | hasGroup 4 hand                     = FourOfAKind
  | hasGroup 3 hand && hasGroup 2 hand  = FullHouse
  | hasGroup 3 hand                     = ThreeOfAKind
  | hasTwoPair hand                     = TwoPair
  | hasGroup 2 hand                     = OnePair
  | otherwise                           = HighCard

hasGroup :: Int -> Hand -> Bool
hasGroup groupSize hand
  = case DL.group . DL.sort . handList $ hand of
      hand -> any ((== groupSize) . length) hand

handList :: Hand -> [Card]
handList (Hand card1 card2 card3 card4 card5 _)
  = [card1, card2, card3, card4, card5]

hasTwoPair :: Hand -> Bool
hasTwoPair hand
  = case DL.group . DL.sort . handList $ hand of
      hand -> length (filter ((== 2) . length) hand) == 2

-- Input Parsing

bidParser :: TPS.Parser Bid
bidParser = read <$> TP.many1 TP.digit

cardsParser :: TPS.Parser [Card]
cardsParser = TP.many1 (Card <$> TP.alphaNum)

fileParser :: TPS.Parser [Hand]
fileParser = TP.sepBy1 handParser TP.newline

handParser :: TPS.Parser Hand
handParser = do
  [card1, card2, card3, card4, card5] <- cardsParser
  TP.spaces
  bid <- bidParser
  return $ Hand card1 card2 card3 card4 card5 bid

main :: IO ()
main
    = print
    . sum
    . fmap (CA.liftA2 (*) fst (bid . snd))
    . zip [1..]
    . DL.sort
  =<< U.unsafeParseFromFile fileParser "camel-cards.txt"
