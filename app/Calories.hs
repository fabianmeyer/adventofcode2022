module Calories where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List as L
import qualified Data.List.Split as L

type Calories = Integer
type Elf = [Calories]

runInput :: T.Text -> [T.Text]
runInput input = [
        "Top: " <> (T.pack . show $ head sortedElfCalories), 
        "Sum Top 3: " <> (T.pack . show . sum $ L.take 3 sortedElfCalories)
    ]
    where sortedElfCalories = sortedCalorySum $ parseElfs input

parseElfs :: T.Text -> [Elf]
parseElfs input = parseElf <$> (L.splitOn [""] $ T.lines input)

parseElf :: [T.Text] -> Elf
parseElf = map parseCalories

parseCalories :: T.Text -> Calories
parseCalories = (read . T.unpack) 

sortedCalorySum :: [Elf] -> [Calories]
sortedCalorySum = L.sortBy (flip compare) . map sum

    