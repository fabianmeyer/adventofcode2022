module Calories where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List as L
import qualified Data.List.Split as L
import Prettyprinter

-- Data Types
type Calories = Integer
type Elf = [Calories]

data Result = Result {
    mostCalories :: Calories,
    sumTopThreeCalories :: Calories
} 

-- Result pretty printing
instance Show Result where    
    show (Result {..}) = show . vcat $ (\(key, value) -> fillBreak 12 (pretty key <> ":") <+> pretty value) <$> kvs
        where kvs = [("Top" :: T.Text, show mostCalories), ("Sum Top 3", show sumTopThreeCalories)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseElfs input

-- Logic
solve :: [Elf] -> Result
solve elfs = Result topCalories sumTopThreeCalories
    where sortedElfCalories = sortedCalorySum elfs
          topCalories = head sortedElfCalories
          sumTopThreeCalories = sum $ L.take 3 sortedElfCalories

sortedCalorySum :: [Elf] -> [Calories]
sortedCalorySum = L.sortBy (flip compare) . map sum

-- Parsing
parseElfs :: T.Text -> [Elf]
parseElfs input = parseElf <$> (L.splitOn [""] $ T.lines input)

parseElf :: [T.Text] -> Elf
parseElf = map parseCalories

parseCalories :: T.Text -> Calories
parseCalories = (read . T.unpack) 