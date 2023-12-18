module LensLibrary2 where

import qualified Data.Char   as DC
import qualified Data.List   as DL
import qualified Text.Parsec as TP
import qualified Utils       as U

-- | Parsing

type Parser = TP.Parsec String ()

fileParser :: Parser [Step]
fileParser = TP.sepBy stepParser $ TP.char ','

stepParser :: Parser Step
stepParser = TP.many1 $ TP.noneOf ","

-- | Logic

replace :: Int -> a -> [a] -> [a]
replace index element list
  = take index list ++ [element] ++ drop (index + 1) list

type Label = String

hash :: Label -> Int
hash = DL.foldl' (\hash -> (`mod`256) . (*17) . (+hash) . fromEnum) 0

type Box = [Lens]

type FocalLength = Int

data Lens = Lens {
    label       :: Label
  , focalLength :: FocalLength
  } deriving (Eq, Show)

boxesPower :: [Box] -> Int
boxesPower = sum . zipWith (*) [1..] . fmap boxPower

boxPower :: Box -> Int
boxPower = sum . zipWith (*) [1..] . fmap focalLength

insertLens :: Lens -> Box -> Box
insertLens newLens@(Lens lensLabel _) box
  = case DL.findIndex ((== lensLabel) . label) box of
      Just index -> replace index newLens box
      Nothing    -> box ++ [newLens]

removeLabel :: Label -> Box -> Box
removeLabel = filter . (. label) . (/=)

data Operation = Insert FocalLength | Remove deriving (Eq, Show)

type Step = String

applyBox :: Step -> Box -> Box
applyBox step box = case operation step of
  Insert focalLength -> insertLens (Lens (labelFromStep step) focalLength) box
  Remove             -> removeLabel (labelFromStep step) box

applyBoxes :: Step -> [Box] -> [Box]
applyBoxes step boxes
  = replace boxIndex (applyBox step (boxes !! boxIndex)) boxes
  where
    boxIndex = hash . labelFromStep $ step

labelFromStep :: Step -> Label
labelFromStep = takeWhile DC.isAlpha

operation :: Step -> Operation
operation step = case dropWhile DC.isAlpha $ step of
  ('=':focalLength) -> Insert . read $ focalLength
  "-"               -> Remove
  other             -> error $ "Invalid step " ++ show other ++ "."

main :: IO ()
main
    = print
    . boxesPower
    . DL.foldl' (flip applyBoxes) (replicate 256 [])
  =<< U.unsafeParseFromFile fileParser "lens-library.txt"
