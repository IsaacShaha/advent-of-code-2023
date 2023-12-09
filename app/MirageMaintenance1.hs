module MirageMaintenance1 where

import           Data.Sequence      (Seq (Empty, (:<|), (:|>)), fromList)
import           Text.Parsec        (char, digit, many1, newline, option,
                                     sepBy1)
import           Text.Parsec.String (Parser)
import           Utils              (unsafeParseFromFile)

-- Input Parsing

fileParser :: Parser (Seq (Seq Int))
fileParser = fromList <$> sepBy1 valueHistoryParser newline

intParser :: Parser Int
intParser = fmap read $ (:) <$> option ' ' (char '-') <*> (many1 digit)

valueHistoryParser :: Parser (Seq Int)
valueHistoryParser = fromList <$> sepBy1 intParser (char ' ')

-- Logic

diffs :: Seq Int -> Seq Int
diffs Empty              = Empty
diffs (n :<| Empty)      = Empty
diffs (n1 :<| n2 :<| ns) = n2 - n1 :<| diffs (n2 :<| ns)

next :: Seq Int -> Int
next ns
  | all (== 0) ns = 0
  | otherwise = seqLast ns + (next . diffs $ ns)

seqLast :: Seq a -> a
seqLast (_ :|> a) = a

main :: IO ()
main
    = unsafeParseFromFile fileParser "mirage-maintenance.txt"
  >>= print . sum . fmap next
