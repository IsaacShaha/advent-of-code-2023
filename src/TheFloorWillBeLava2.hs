module TheFloorWillBeLava2 where

import qualified Control.Applicative as CA
import           Control.Monad.State (State)
import qualified Control.Monad.State as CMS
import qualified Data.List           as DL
import           Data.Maybe          (Maybe)
import qualified Data.Maybe          as DM
import           Data.Vector         (Vector, (!), (!?))
import qualified Data.Vector         as DV
import           Prelude             hiding (Either (..))
import qualified Text.Parsec         as TP
import qualified Utils               as U

-- | Input Parsing

type Parser = TP.Parsec String ()

fileParser :: Parser (Vector (Vector Tile))
fileParser = DV.fromList <$> TP.sepBy1 rowParser TP.newline

rowParser :: Parser (Vector Tile)
rowParser = DV.fromList <$> TP.many1 tileParser

tileParser :: Parser Tile
tileParser = charToTile <$> TP.oneOf ".-\\/|"

-- | Logic

nub :: Ord a => [a] -> [a]
nub [] = []
nub xs
  = CA.liftA2 (DL.foldl' $ \xs x -> if x == head xs then xs else x : xs)
      ((: []) . head) tail
  . DL.sort $ xs

data Direction = Down | Left | Right | Up deriving (Eq, Show)

opposite :: Direction -> Direction
opposite Down  = Up
opposite Left  = Right
opposite Right = Left
opposite Up    = Down

data Position = Position
  { row    :: Int
  , column :: Int
  } deriving (Eq, Ord, Show)

data Move = Move
  { position  :: Position
  , direction :: Direction
  } deriving (Eq, Show)

exit :: Move -> Move
exit = CA.liftA2 Move nextPosition (opposite . direction)

nextPosition :: Move -> Position
nextPosition (Move (Position row column) Down)  = Position (row + 1) column
nextPosition (Move (Position row column) Left)  = Position row (column - 1)
nextPosition (Move (Position row column) Right) = Position row (column + 1)
nextPosition (Move (Position row column) Up)    = Position (row - 1) column

data Segment = Segment
  { entry1    :: Move
  , entry2    :: Move
  , positions :: [Position]
  } deriving (Eq, Show)

data Tile
  = Empty
  | HorizontalSplitter
  | LeftLeaningMirror
  | RightLeaningMirror
  | VerticalSplitter deriving (Enum)

instance Show Tile where
  show = (: []) . tileToChar

charToTile :: Char -> Tile
charToTile '.'  = Empty
charToTile '-'  = HorizontalSplitter
charToTile '\\' = LeftLeaningMirror
charToTile '/'  = RightLeaningMirror
charToTile '|'  = VerticalSplitter

nextDirections :: Tile -> Direction -> [Direction]
nextDirections LeftLeaningMirror Down   = [Right]
nextDirections LeftLeaningMirror Left   = [Up]
nextDirections LeftLeaningMirror Right  = [Down]
nextDirections LeftLeaningMirror Up     = [Left]
nextDirections RightLeaningMirror Down  = [Left]
nextDirections RightLeaningMirror Left  = [Down]
nextDirections RightLeaningMirror Right = [Up]
nextDirections RightLeaningMirror Up    = [Right]
nextDirections HorizontalSplitter Down  = [Left, Right]
nextDirections HorizontalSplitter Up    = [Left, Right]
nextDirections VerticalSplitter Left    = [Down, Up]
nextDirections VerticalSplitter Right   = [Down, Up]
nextDirections _ direction              = [direction]

nextMoves :: Vector (Vector Tile) -> Move -> [Move]
nextMoves tiles move@(Move _ direction)
  = CA.liftA2 (zipWith Move) repeat
      (DM.maybe [] (flip nextDirections direction) . positionToTile tiles)
  . nextPosition $ move

positionToTile :: Vector (Vector Tile) -> Position -> Maybe Tile
positionToTile tiles (Position row column) = (!? column) =<< (!? row) tiles

reach :: Vector (Vector Tile) -> Move -> [Position]
reach tiles = nub . flip CMS.evalState [] . go tiles
  where
    go :: Vector (Vector Tile)
       -> Move
       -> State [Segment] [Position]
    go tiles move@(Move (Position row column) direction) = do
      log <- CMS.get
      let segment' = segment tiles move
      case () of
        _ | elem segment' log -> return []
          | otherwise -> do
              CMS.put $ segment' : log
              let validConnectors
                    = filter (DM.isJust . positionToTile tiles)
                    . fmap position
                    $ [entry1', entry2']
                  entry1' = entry1 segment'
                  entry2' = entry2 segment'
                  exit' = if move == entry1 segment'
                          then exit entry2'
                          else exit entry1'
                  nextMoves' = nextMoves tiles exit'
              nextResults <- go tiles `mapM` nextMoves'
              return $ validConnectors ++ positions segment' ++ concat nextResults

segment :: Vector (Vector Tile) -> Move -> Segment
segment tiles
  = CA.liftA3 Segment head (exit . last) (DL.sort . fmap position . tail)
  . fmap head . takeWhile ((==1) . length) . iterate (nextMoves tiles . head)
  . (: [])

tileToChar :: Tile -> Char
tileToChar Empty              = '.'
tileToChar HorizontalSplitter = '-'
tileToChar LeftLeaningMirror  = '\\'
tileToChar RightLeaningMirror = '/'
tileToChar VerticalSplitter   = '|'

main :: IO ()
main = do
  tiles <- U.unsafeParseFromFile fileParser "the-floor-will-be-lava.txt"
  let columns = [0..DV.length (tiles ! 0) - 1]
      movesDown = fmap (flip Move Down . Position (-1)) columns
      movesLeft = fmap (flip Move Left . flip Position (length columns)) rows
      movesRight = fmap (flip Move Right . flip Position (-1)) rows
      movesUp = fmap (flip Move Up . Position (length rows)) columns
      rows = [0..DV.length tiles - 1]
  print . maximum . fmap (length . reach tiles) . concat
    $ [movesDown, movesLeft, movesRight, movesUp]
