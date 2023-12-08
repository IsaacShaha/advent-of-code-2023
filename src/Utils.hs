module Utils where

import qualified Data.Either        as DE
import qualified Text.Parsec        as TP
import qualified Text.Parsec.String as TPS

unsafeParseFromFile :: TPS.Parser a -> FilePath -> IO a
unsafeParseFromFile parser filePath
  = DE.fromRight undefined <$> U.unsafeParseFromFile parser filePath
