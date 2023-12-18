module CamelCards2 where

import qualified Control.Applicative as CA
import qualified Data.Char           as DC
import qualified Data.List           as DL
import qualified Text.Parsec         as TP
import qualified Text.Parsec.String  as TPS
import qualified Utils               as U

data Card = Card Char deriving (Eq, Show)

instance Bounded Card where
  minBound = Card 'J'
  maxBound = Card 'A'

instance Enum Card where
    fromEnum (Card 'J') = 0
    fromEnum (Card 'T') = 9
    fromEnum (Card 'Q') = 10
    fromEnum (Card 'K') = 11
    fromEnum (Card 'A') = 12
    fromEnum (Card c)   = DC.digitToInt c - 1

    toEnum 0  = Card 'J'
    toEnum 9  = Card 'T'
    toEnum 10 = Card 'Q'
    toEnum 11 = Card 'K'
    toEnum 12 = Card 'A'
    toEnum i  = Card $ DC.intToDigit $ i + 1

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

maximize :: Hand -> Hand
maximize hand@(Hand _ _ _ _ _ bid) = fromList bid maximized
  where
    biggestGroup = DL.maximumBy compareSize groups
    groups = DL.group . DL.sort $ nonJs
    handList = toList hand
    nonJs = filter (/= Card 'J') handList
    numJs = (5 -) . length $ nonJs
    num2Pairs = length . filter ((== 2) . length) $ groups
    mapJokers :: Card -> [Card] -> [Card]
    mapJokers mapTo = fmap (\card -> if card == Card 'J' then mapTo else card)
    maximized :: [Card]
    maximized
      | numJs == 5 = replicate 5 $ maxBound
      | numJs == 4 = replicate 5 . head $ nonJs
      | numJs == 3 = case length biggestGroup of
         2 -> replicate 5 . head $ nonJs
         1 -> mapJokers (head biggestGroup) handList
      | numJs == 2 = case length biggestGroup of
         3 -> replicate 5 . head $ nonJs
         2 -> mapJokers (head biggestGroup) handList
         1 -> mapJokers (maximum nonJs) handList
      | numJs == 1 = case length biggestGroup of
         4 -> replicate 5 . head $ biggestGroup
         3 -> mapJokers (head biggestGroup) handList
         2 -> case num2Pairs of
           2 -> mapJokers (maximum nonJs) handList
           1 -> mapJokers (head biggestGroup) handList
         1 -> mapJokers (maximum nonJs) handList
      | otherwise = handList

fromList :: Int -> [Card] -> Hand
fromList bid [card1, card2, card3, card4, card5]
  = Hand card1 card2 card3 card4 card5 bid

handType :: Hand -> HandType
handType hand = handType' . maximize $ hand
  where
    handType' :: Hand -> HandType
    handType' hand
      | hasGroup 5 hand                     = FiveOfAKind
      | hasGroup 4 hand                     = FourOfAKind
      | hasGroup 3 hand && hasGroup 2 hand  = FullHouse
      | hasGroup 3 hand                     = ThreeOfAKind
      | hasTwoPair hand                     = TwoPair
      | hasGroup 2 hand                     = OnePair
      | otherwise                           = HighCard

hasGroup :: Int -> Hand -> Bool
hasGroup groupSize hand
  = case DL.group . DL.sort . toList $ hand of
      hand -> any ((== groupSize) . length) hand

hasTwoPair :: Hand -> Bool
hasTwoPair hand
  = case DL.group . DL.sort . toList $ hand of
      hand -> length (filter ((== 2) . length) hand) == 2

toList :: Hand -> [Card]
toList (Hand card1 card2 card3 card4 card5 _)
  = [card1, card2, card3, card4, card5]

-- General Logic

compareSize :: [a] -> [a] -> Ordering
compareSize list1 list2 = compare (length list1) . length $ list2

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
