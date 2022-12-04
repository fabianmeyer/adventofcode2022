module CampCleanup where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Set as S
import Data.Foldable (foldr1)

import Prettyprinter hiding (group)

-- Data Types
data SectionAssignment = SectionAssignment {
    start :: Int,
    end :: Int
} deriving (Show)

type SectionAssignmentPair = (SectionAssignment, SectionAssignment)

data Result = Result {
    containingSectionAssignmentPairs :: Int,
    overlappingSectionAssignmentPairs :: Int
}

-- Result pretty printing
instance Show Result where    
    show (Result {..}) = show . vcat $ (\(key, value) -> fillBreak ((maximum $ T.length . fst <$> kvs) + 3) (pretty key <> ":") <+> pretty value) <$> kvs
        where kvs = [("Containing Section Assignment Pairs" :: T.Text, show containingSectionAssignmentPairs), ("Overlapping Section Assignment Pairs", show overlappingSectionAssignmentPairs)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseSectionAssignmentPair <$> T.lines input

-- Logic
solve :: [SectionAssignmentPair] -> Result
solve parseSectionAssignmentPairs = Result containingSectionAssignmentPairs overlappingSectionAssignmentPairs
    where containingSectionAssignmentPairs = L.length $ filter sectionAssignmentPairContain parseSectionAssignmentPairs
          overlappingSectionAssignmentPairs = L.length $ filter sectionAssignmentPairOverlaps parseSectionAssignmentPairs
        
sectionAssignmentPairContain :: SectionAssignmentPair -> Bool
sectionAssignmentPairContain (first, second) = first `contains` second || second `contains` first

contains :: SectionAssignment -> SectionAssignment -> Bool
contains (SectionAssignment firstStart firstEnd) (SectionAssignment secondStart secondEnd) = firstStart <= secondStart && secondEnd <= firstEnd

sectionAssignmentPairOverlaps :: SectionAssignmentPair -> Bool
sectionAssignmentPairOverlaps (first, second) = first `starts` second || second `starts` first

starts :: SectionAssignment -> SectionAssignment -> Bool
starts (SectionAssignment firstStart firstEnd) (SectionAssignment secondStart secondEnd) = firstStart <= secondStart && secondStart <= firstEnd

-- Parsing
parseSectionAssignmentPair :: T.Text -> SectionAssignmentPair
parseSectionAssignmentPair line = (parseSectionAssignment first, parseSectionAssignment second)
    where [first, second] = T.splitOn "," line

parseSectionAssignment :: T.Text -> SectionAssignment
parseSectionAssignment line = SectionAssignment (read $ T.unpack start) (read $ T.unpack end)
    where [start, end] = T.splitOn "-" line 