module LensLibrary1 where

import qualified Data.Char     as DC
import qualified Data.Foldable as DF
import qualified Data.List     as DL
import qualified Text.Parsec   as TP
import qualified Utils         as U

import           Debug.Trace

-- | Parsing

type Parser = TP.Parsec String ()

fileParser :: Parser [Step]
fileParser = TP.sepBy stepParser $ TP.char ','

stepParser :: Parser Step
stepParser = TP.many1 $ TP.noneOf ","

-- | Logic

type FocalLength = Int
type Label = String
data Lens = Lens Label FocalLength deriving (Eq, Show)
data Operation = Insert FocalLength | Remove deriving (Eq, Show)
type Step = String

applyBox :: Step -> [Lens] -> [Lens]
applyBox step box = case operation step of
  Insert focalLength -> insert (Lens (label step) focalLength) box
  Remove             -> undefined

applyBoxes :: Step -> [[Lens]] -> [[Lens]]
applyBoxes step boxes
  = take boxIndex boxes
 ++ applyBox step (boxes !! boxIndex)
  : drop (boxIndex + 1) boxes
  where
    boxIndex = hash step

hash :: Step -> Int
hash = DF.foldl' (\hash -> (`mod`256) . (*17) . (+hash) . fromEnum) 0

insert :: Lens -> [Lens] -> [Lens]
insert lens lenses = case DL.elemIndex lens lenses of
  Just index -> take index lenses ++ [lens] ++ drop (index + 1) lenses
  Nothing    -> lenses ++ [lens]

label :: Step -> Label
label = takeWhile DC.isAlpha

operation :: Step -> Operation
operation step = case dropWhile DC.isAlpha $ step of
  ('=':focalLength) -> Insert . read $ focalLength
  "-"               -> Remove
  other             -> error $ "Invalid step " ++ show other ++ "."

main :: IO ()
main = do
  steps <- U.unsafeParseFromFile fileParser "lens-library.txt"
  let boxes = replicate 256 [] :: [[Lens]]
  print $ applyBoxes (steps !! 0) boxes
