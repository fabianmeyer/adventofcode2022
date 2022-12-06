module TuningTrouble where

import qualified Data.Text as T
import qualified Data.List as L
import PrettyPrint

data Result = Result {
    firstSignal :: Int,
    firstMessage :: Int
} 

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("First Signal" :: T.Text, show firstSignal), 
        ("First Message", show firstMessage)]

-- Entry point
runInput :: T.Text -> Result
runInput = solve 

-- Logic
solve :: T.Text -> Result
solve input = Result (findStart 4 indexedInput) (findStart 14 indexedInput)
    where indexedInput = zip [1 ..] . T.unpack $ input

findStart :: Int -> [(Int, Char)] -> Int
findStart length list | maxLetterOccurence == 1 = marker
    where sublist = L.take length list
          maxLetterOccurence =  maximum $ L.length <$> (L.group . L.sort $ snd <$> sublist)
          (marker, _) = L.last sublist
findStart length (_ : tail) = findStart length tail
findStart _ [] = error "empty"
