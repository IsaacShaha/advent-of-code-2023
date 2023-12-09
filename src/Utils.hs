module Utils where

import qualified Data.Either        as DE
import qualified Text.Parsec        as TP
import qualified Text.Parsec.String as TPS

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

third3 :: (a, b, c) -> c
third3 (a, b, c) = c

unsafeParseFromFile :: TPS.Parser a -> FilePath -> IO a
unsafeParseFromFile parser filePath
  = DE.fromRight undefined <$> TPS.parseFromFile parser ("input/" ++ filePath)
