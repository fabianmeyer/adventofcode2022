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
    where sortedElfCalories = sortedCalories $ parseElfs input

parseElfs :: T.Text -> [Elf]
parseElfs input = map (read . T.unpack) <$> elfLines
    where inputLines = T.lines input
          elfLines = L.splitOn [""] inputLines

sortedCalories :: [Elf] -> [Calories]
sortedCalories elfs = L.sortBy (flip compare) $ sum <$> elfs
    