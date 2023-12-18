module HauntedWasteland1 where

import qualified Control.Applicative as CA
import qualified Data.Map            as DM
import           Prelude             hiding (Left, Right)
import qualified Text.Parsec         as TP
import qualified Text.Parsec.String  as TPS
import qualified Utils               as U

data Direction = Left | Right deriving Show

data Node = Junction {
  name  :: String
, left  :: String
, right :: String
} deriving (Eq, Show)

next :: DM.Map String Node -> Node -> Direction -> Node
next nodes junction Left  = (DM.!) nodes $ left junction
next nodes junction Right = (DM.!) nodes $ right junction

path :: [Direction] -> DM.Map String Node -> Node -> Node -> [Node]
path (direction:directions) nodes start end
  | start == end = start : []
  | otherwise = start : path directions nodes (next nodes start direction) end

-- Input Parsing

fileParser :: TPS.Parser ([Direction], DM.Map String Node)
fileParser = do
  directions <- directionsParser
  TP.spaces
  nodes
     <- ( DM.fromList
        . fmap (CA.liftA2 (,) name id))
    <$> TP.sepBy1 nodeParser TP.newline
  return (directions, nodes)

directionsParser :: TPS.Parser [Direction]
directionsParser
    = cycle
  <$> TP.many1 (TP.char 'L' *> return Left TP.<|> TP.char 'R' *> return Right)

nodeParser :: TPS.Parser Node
nodeParser = do
  name <- TP.many1 TP.letter
  TP.string " = "
  [left, right]
   <- TP.between (TP.char '(') (TP.char ')')
    $ TP.sepEndBy1 (TP.many1 TP.letter)
    $ TP.string ", "
  return $ Junction name left right

main :: IO ()
main = do
  (directions, nodes)
   <- U.unsafeParseFromFile fileParser "haunted-wasteland.txt"
  print
    . subtract 1
    . length
    . path directions nodes ((DM.!) nodes "AAA")
    . (DM.!) nodes
    $ "ZZZ"
