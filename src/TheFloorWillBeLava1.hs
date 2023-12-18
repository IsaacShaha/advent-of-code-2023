module TheFloorWillBeLava1 where

import qualified Control.Applicative as CA
import           Control.Monad.State (State)
import qualified Control.Monad.State as CMS
import qualified Data.List           as DL
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

data Direction = Down | Left | Right | Up deriving (Eq, Show)

nextIndex :: (Int, Int, Direction) -> (Int, Int)
nextIndex (row, column, Down)  = (row + 1, column)
nextIndex (row, column, Left)  = (row, column - 1)
nextIndex (row, column, Right) = (row, column + 1)
nextIndex (row, column, Up)    = (row - 1, column)

data Tile
  = Empty
  | HorizontalSplitter
  | LeftLeaningMirror
  | RightLeaningMirror
  | VerticalSplitter deriving (Eq)

instance Show Tile where
  show = (: []) . tileToChar

charToTile :: Char -> Tile
charToTile '.'  = Empty
charToTile '-'  = HorizontalSplitter
charToTile '\\' = LeftLeaningMirror
charToTile '/'  = RightLeaningMirror
charToTile '|'  = VerticalSplitter

energized :: Vector (Vector Tile) -> [(Int, Int)]
energized tiles = CMS.evalState (go tiles (0, 0, Right)) []
  where
    go :: Vector (Vector Tile)
       -> (Int, Int, Direction)
       -> State [(Int, Int, Direction)] [(Int, Int)]
    go tiles move@(row, column, direction) = do
      log <- CMS.get
      case () of
        _ | tiles !? row == Nothing          -> return []
          | tiles ! row !? column == Nothing -> return []
          | elem move log                    -> return []
          | otherwise                        -> do
            CMS.put (move : log)
            nextResults <- go tiles `mapM` nextMoves tiles move
            return $ (row, column) : concat nextResults

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

nextMoves :: Vector (Vector Tile)
          -> (Int, Int, Direction)
          -> [(Int, Int, Direction)]
nextMoves tiles move@(row, column, direction)
    = zipWith (CA.liftA2 (,,) fst snd)
  =<< fmap (nextIndex . (,,) row column)
    $ nextDirections (tiles!row!column) direction

tileToChar :: Tile -> Char
tileToChar Empty              = '.'
tileToChar HorizontalSplitter = '-'
tileToChar LeftLeaningMirror  = '\\'
tileToChar RightLeaningMirror = '/'
tileToChar VerticalSplitter   = '|'

main :: IO ()
main
    = print
    . length
    . DL.nub
    . energized
  =<< U.unsafeParseFromFile fileParser "the-floor-will-be-lava.txt"
