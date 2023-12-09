module HauntedWasteland2 where

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

ghostPath :: [[Node]] -> [[Node]]
ghostPath paths
  | all . fmap (== 'Z') heads = heads : []
  | otherwise = (:) <$> heads <*> pure $ fmap tail paths
  where
    heads = fmap head paths

next :: DM.Map String Node -> Node -> Direction -> Node
next nodes junction Left  = (DM.!) nodes $ left junction
next nodes junction Right = (DM.!) nodes $ right junction

path :: [Direction] -> DM.Map String Node -> Node  -> [Node]
path (direction:directions) nodes start
  = start : path directions nodes (next nodes start direction)

-- Input Parsing

fileParser :: TPS.Parser ([Direction], DM.Map String Node)
fileParser = do
  directions <- directionsParser
  TP.spaces
  nodes
     <- ( DM.fromList
        . fmap (CA.liftA2 (,) name id) )
    <$> TP.sepBy1 nodeParser TP.newline
  return (directions, nodes)

directionsParser :: TPS.Parser [Direction]
directionsParser
    = cycle
  <$> TP.many1 (TP.char 'L' *> return Left TP.<|> TP.char 'R' *> return Right)

nodeParser :: TPS.Parser Node
nodeParser = do
  name <- TP.many1 TP.alphaNum
  TP.string " = "
  [left, right]
   <- TP.between (TP.char '(') (TP.char ')')
    $ TP.sepEndBy1 (TP.many1 TP.alphaNum)
    $ TP.string ", "
  return $ Junction name left right

main :: IO ()
main = do
  (directions, nodes) <- U.unsafeParseFromFile fileParser "haunted-wasteland.txt"
  let startNodes = filter ((== 'A') . last . name) . fmap snd . DM.toList $ nodes
  print
    . fmap (path directions nodes)
    $ startNodes
