module HauntedWasteland2 where

import qualified Data.Array         as DA
import qualified Data.Graph         as DG
import qualified Data.List          as DL
import qualified Data.Maybe         as DM
import           Prelude            hiding (Left, Right)
import qualified Text.Parsec        as TP
import qualified Text.Parsec.String as TPS
import qualified Utils              as U

data Direction = Left | Right deriving (Show)

-- This would be done by automatically deriving Enum, but it's nice to make it
-- explicit when relying on specific values.
instance Enum Direction where
  fromEnum Left  = 0
  fromEnum Right = 1
  toEnum 0 = Left
  toEnum 1 = Right

type Key = String
type Graph = ( DG.Graph
             , DG.Vertex -> (String, Key, [Key])
             , Key -> Maybe DG.Vertex          )

graphFromTuples :: [(String, String, String)] -> Graph
graphFromTuples
  = DG.graphFromEdges
  . fmap (\(name, left, right) -> (name, name, [left, right]))

next :: Graph -> Direction -> DG.Vertex -> DG.Vertex
next graph direction vertex
  = (!! fromEnum direction) . verticesFromVertex graph $ vertex

pathTo :: Graph -> [Direction] -> [DG.Vertex] -> DG.Vertex -> [DG.Vertex]
pathTo graph (direction:nextDirections) ends start
  | elem start ends = start : []
  | otherwise
      = (start :)
      . pathTo graph nextDirections ends
      . next graph direction
      $ start

unsafeVertexFromKey :: Graph -> Key -> DG.Vertex
unsafeVertexFromKey (_, _, vertexFromKey) = DM.fromJust . vertexFromKey

verticesFromVertex :: Graph -> DG.Vertex -> [DG.Vertex]
verticesFromVertex (graph, _, _) = (graph DA.!)

-- Input Parsing

fileParser :: TPS.Parser ([Direction], [(String, String, String)])
fileParser = do
  directions <- directionsParser
  TP.spaces
  nodes <- TP.sepBy1 nodeParser TP.newline
  return (directions, nodes)

directionsParser :: TPS.Parser [Direction]
directionsParser
    = cycle
  <$> TP.many1 (TP.char 'L' *> return Left TP.<|> TP.char 'R' *> return Right)

nodeParser :: TPS.Parser (String, String, String)
nodeParser = do
  name <- TP.many1 TP.alphaNum
  TP.string " = "
  [left, right]
   <- TP.between (TP.char '(') (TP.char ')')
    $ TP.sepEndBy1 (TP.many1 TP.alphaNum)
    $ TP.string ", "
  return $ (name, left, right)

main :: IO ()
main = do
  (directions, nodeTuples)
   <- U.unsafeParseFromFile fileParser "haunted-wasteland.txt"
  let ends = unsafeVertexFromKey graph <$> filter ((== 'Z') . last) keys
      graph = graphFromTuples nodeTuples
      keys = U.fst3 <$> nodeTuples
      starts = unsafeVertexFromKey graph <$> filter ((== 'A') . last) keys
  print
    . DL.foldl1'
      lcm
    . fmap (subtract 1 . length)
    . fmap (pathTo graph directions ends)
    $ starts
