module TreetopTreeHouse where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Foldable (foldl')
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P
import Data.Maybe (catMaybes)
import PrettyPrint

data Tree a = Tree Int a deriving (Show) 
type TreeLine a = [Tree a]
type Forest a = [TreeLine a]

data Result = Result {
    visibleCount :: Int,
    reachableCount :: Int
}

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("Visible Count" :: T.Text, show visibleCount), 
        ("Reachable Count", show reachableCount)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseInput input

-- Logic
solve :: Forest () -> Result
solve forest = Result visibilityCount reachableCount
    where visibilityForest = mkVisibilityForest forest
          visibilitySteps = iterate (rotate . lineOfSightForest) visibilityForest
          visibility = visibilitySteps !! 4
          visibilityCount = L.length $ filter (\(Tree _ v) -> v) $ concat visibility
          reachableForest = mkReachableForest forest
          reachableSteps = iterate (rotate . viewingDistanceForest) reachableForest
          reachable = reachableSteps !! 4
          reachableCount = maximum $ (\(Tree _ v) -> v) <$> concat reachable

rotate :: [[a]] -> [[a]]
rotate = L.transpose . map reverse

mkVisibilityForest :: Forest () -> Forest Bool
mkVisibilityForest forest = map (\(Tree height _) -> Tree height False) <$> forest

mkReachableForest :: Forest () -> Forest Int
mkReachableForest forest = map (\(Tree height _) -> Tree height 1) <$> forest


lineOfSightForest :: Forest Bool -> Forest Bool
lineOfSightForest forest = lineOfSightTreeLine (-1) <$> forest

lineOfSightTreeLine :: Int -> TreeLine Bool -> TreeLine Bool
lineOfSightTreeLine maxHeight ((Tree height _) : tail) | height > maxHeight = (Tree height True) : lineOfSightTreeLine height tail
lineOfSightTreeLine maxHeight (head : tail) = head : lineOfSightTreeLine maxHeight tail
lineOfSightTreeLine _ [] = []

viewingDistanceForest :: Forest Int -> Forest Int
viewingDistanceForest forest = viewingDistanceTreeLine <$> forest

viewingDistanceTreeLine :: TreeLine Int -> TreeLine Int
viewingDistanceTreeLine ((Tree height cnt) : tail) = (Tree height (cnt * viewingDistance height tail)) : viewingDistanceTreeLine tail
viewingDistanceTreeLine [] = []

viewingDistance :: Int -> TreeLine Int -> Int
viewingDistance height ((Tree th _) : tail) | th >= height = 1
viewingDistance height (_ : []) = 1
viewingDistance height (_ : tail) = viewingDistance height tail + 1
viewingDistance _ [] = 0

-- Parsing
parseInput :: T.Text -> Forest ()
parseInput lines = parseTreeLine <$> T.lines lines

parseTreeLine :: T.Text -> TreeLine ()
parseTreeLine line = parseTree <$> T.unpack line

parseTree :: Char -> Tree ()
parseTree c = Tree (read . pure $ c) ()