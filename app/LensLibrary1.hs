module LensLibrary1 where

import qualified Data.Foldable as DF
import qualified Text.Parsec   as TP
import qualified Utils         as U

-- | Parsing

type Parser = TP.Parsec String ()

fileParser :: Parser [String]
fileParser = TP.sepBy stepParser $ TP.char ','

stepParser :: Parser String
stepParser = TP.many1 $ TP.noneOf ","

-- | Logic

hash :: String -> Int
hash = DF.foldl' (\hash -> (`mod`256) . (*17) . (+hash) . fromEnum) 0

main :: IO ()
main
    = print
    . sum
    . fmap hash
  =<< U.unsafeParseFromFile fileParser "lens-library.txt"
