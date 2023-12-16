module TheFloorWillBeLava2 where

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

data Direction = Down | Left | Right | Up deriving (Bounded, Enum, Eq, Show)

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

energized :: Vector (Vector Tile) -> Direction -> Int -> [(Int, Int)]
energized tiles direction index = CMS.evalState (go tiles (row, column, direction)) []
  where
    (row, column) = case direction of
      Down  -> (0, index)
      Left  -> (index, DV.length (DV.head tiles) - 1)
      Right -> (index, 0)
      Up    -> (DV.length tiles - 1, index)
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
  =<< fmap (nextIndex . (row, column,))
    $ nextDirections (tiles!row!column) direction

tileToChar :: Tile -> Char
tileToChar Empty              = '.'
tileToChar HorizontalSplitter = '-'
tileToChar LeftLeaningMirror  = '\\'
tileToChar RightLeaningMirror = '/'
tileToChar VerticalSplitter   = '|'

main :: IO ()
main = do
  tiles <- U.unsafeParseFromFile fileParser "the-floor-will-be-lava.txt"
  let directions = [minBound .. maxBound] :: [Direction]
      horizontalStartIndices = [0 .. DV.length tiles - 1]
      verticalStartIndices = [0 .. DV.length (DV.head tiles) - 1]
      starts
        = concat
        . zipWith
            (zip . repeat)
            directions
        $ [ verticalStartIndices
          , horizontalStartIndices
          , horizontalStartIndices
          , verticalStartIndices ]
  -- print
  --   . maximum
  --   . fmap length
  --   . fmap nub
  --   . fmap (uncurry . energized $ tiles)
  --   $ starts
  print $ DL.sort $ energized tiles Down 3
  print $ nub $ DL.sort $ energized tiles Down 3

-- [(9,7),(9,5),(9,1),(8,7),(8,6),(8,5),(8,4),(8,3),(8,2),(8,1),(7,7),(7,6),(7,5),(7,4),(7,3),(7,2),(7,1),(7,0),(6,7),(6,6),(6,5),(6,4),(6,3),(6,1),(5,6),(5,5),(5,3),(5,1),(4,6),(4,5),(4,3),(4,1),(3,6),(3,5),(3,3),(3,1),(2,9),(2,8),(2,7),(2,6),(2,5),(2,3),(2,1),(1,5),(1,3),(1,1),(0,5),(0,4),(0,3),(0,2),(0,1),(0,3)]
