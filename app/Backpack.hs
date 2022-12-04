module Backpack where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Set as S
import qualified Data.Char as C
import Data.Foldable (foldr1)

import Prettyprinter hiding (group)

type Item = Char
type Compartment = S.Set Item
data Backpack = Backpack {
    first :: Compartment,
    second :: Compartment
}  deriving (Show)

data Result = Result {
    sumOfDuplicatPriorities :: Int,
    sumOfBadgePriorities :: Int
}

-- Result pretty printing
instance Show Result where    
    show (Result {..}) = show . vcat $ (\(key, value) -> fillBreak ((maximum $ T.length . fst <$> kvs) + 3) (pretty key <> ":") <+> pretty value) <$> kvs
        where kvs = [("Sum of duplicat priorities" :: T.Text, show sumOfDuplicatPriorities), ("sum of badge priorities", show sumOfBadgePriorities)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseBackpack <$> T.lines input

-- Logic
solve :: [Backpack] -> Result
solve backpacks = Result (sum $ priority <$> pairs) (sum $ priority <$> badges)
    where pairs = backpacks >>= findPairs
          groups = group backpacks
          badges = badge <$> groups

findPairs :: Backpack -> [Item]
findPairs (Backpack first second) = S.toList $ S.intersection first second

priority :: Item -> Int
priority c | C.isLower c = C.ord c - C.ord 'a' + 1
priority c = C.ord c - C.ord 'A' + 27

group :: [Backpack] -> [[Backpack]]
group = L.chunksOf 3

badge :: [Backpack] -> Item
badge backpacks = L.head . S.toList $ foldr1 S.intersection $ allItems <$> backpacks

allItems :: Backpack -> S.Set Item
allItems (Backpack first second) = S.union first second 

-- Parsing
parseBackpack :: T.Text -> Backpack
parseBackpack line = Backpack (S.fromList $ T.unpack first) (S.fromList $ T.unpack second)
    where (first, second) = T.splitAt (T.length line `div` 2) line

