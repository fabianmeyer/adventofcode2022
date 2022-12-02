module Calories where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List as L
import qualified Data.List.Split as L
import Prettyprinter

type Calories = Integer
type Elf = [Calories]

data Result = Result {
    mostCalories :: Calories,
    sumTopThreeCalories :: Calories
} 

instance Show Result where    
    show (Result {..}) = show . vcat $ (\(key, value) -> fillBreak 12 (pretty key <> ":") <+> pretty value) <$> kvs
        where kvs = [("Top" :: T.Text, show mostCalories), ("Sum Top 3", show sumTopThreeCalories)]

runInput :: T.Text -> Result
runInput input = Result (head sortedElfCalories) (sum $ L.take 3 sortedElfCalories)
    where sortedElfCalories = sortedCalorySum $ parseElfs input

parseElfs :: T.Text -> [Elf]
parseElfs input = parseElf <$> (L.splitOn [""] $ T.lines input)

parseElf :: [T.Text] -> Elf
parseElf = map parseCalories

parseCalories :: T.Text -> Calories
parseCalories = (read . T.unpack) 

sortedCalorySum :: [Elf] -> [Calories]
sortedCalorySum = L.sortBy (flip compare) . map sum