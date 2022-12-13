module DistressSignal where

import qualified Data.Text as T
import PrettyPrint (printKeyValuePairs)
import Debug.Trace (trace)
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P
import Text.Parsec ((<|>))
import Data.Maybe (catMaybes)
import Data.List ((\\))
import qualified Data.List as L

data Packet = Single Int | Multiple Signal deriving (Eq)
type Signal = [Packet]
type SignalPair = (Signal, Signal)
type Signals = [SignalPair] 

data Order = Correct | Incorrect | Unknown deriving (Show, Eq)

instance Show Packet where 
    show (Single s) = show s
    show (Multiple s) = show s

data Result = Result {
    pt1 :: Int,
    pt2 :: Int
} 

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("Sum of indices" :: T.Text, show pt1),
        ("Decoder key" :: T.Text, show pt2)
        ]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseInput input

-- Logic
solve :: Signals -> Result
solve signals = Result pt1 pt2
    where pt1 = solvePt1 signals
          pt2 = solvePt2 signals

solvePt1 :: Signals -> Int
solvePt1 signals = sumIndicesCorrectPairs
    where signalPairCheckResult = uncurry checkSignal <$> signals
          indexedMergedSignalPairs = zip3 ([1..] :: [Int]) signalPairCheckResult signals
          sumIndicesCorrectPairs = sum $ indexedMergedSignalPairs >>= (\(idx, ord, _) -> if ord == Correct then [idx] else []) 

solvePt2 :: Signals -> Int
solvePt2 signals = sep1pos * sep2pos
    where sep1 = [Multiple [Single 2]]
          sep2 = [Multiple [Single 6]]
          signals' = sep1 : sep2 : (signals >>= \(l, r) -> [l, r])
          result = zip [1.. ] $ L.sortBy (\l r -> if (checkSignal l r == Correct) then LT else GT) signals'
          sep1pos = fst . head $ L.filter (\(idx, l) -> l == sep1) result
          sep2pos = fst . head $ L.filter (\(idx, l) -> l == sep2) result          

checkSignal :: Signal -> Signal -> Order
-- If both values are integers, the lower integer should come first. 
-- If the left integer is lower than the right integer, the inputs are in the right order. 
checkSignal (Single l : ltail) (Single r : rtail) | l < r = Correct
-- If the left integer is higher than the right integer, the inputs are not in the right order. 
checkSignal (Single l : ltail) (Single r : rtail) | l > r = Incorrect
-- Otherwise, the inputs are the same integer; continue checking the next part of the input.
checkSignal (Single _ : ltail) (Single _ : rtail) = checkSignal ltail rtail
-- If exactly one value is an integer, convert the integer to a list which contains that integer 
-- as its only value, then retry the comparison. 
checkSignal (l@(Single _) : ltail) r@((Multiple _) : _) = checkSignal (Multiple [l] : ltail) r
checkSignal l@(Multiple _ : ltail) (r@(Single _) : rtail) = checkSignal l (Multiple [r]: rtail)
-- If both values are lists, compare the first value of each list, then the second value, and so on. 
checkSignal ((Multiple l) : ltail) ((Multiple r) : rtail) = case checkSignal l r of 
        Unknown -> checkSignal ltail rtail
        it -> it
-- If the left list runs out of items first, the inputs are in the right order.
checkSignal [] (_ : _) = Correct
-- If the right list runs out of items first, the inputs are not in the right order.
checkSignal (_ : _) [] = Incorrect
--  If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
checkSignal [] [] = Unknown


-- Parsing
parseInput :: T.Text -> Signals
parseInput lines = case eitherCommands of 
            Right commands -> commands
            Left err -> error $ show err
    where parser = P.sepEndBy1 parseSignalPair P.endOfLine
          (eitherCommands) = P.parse parser "input" lines

parseSignalPair :: P.Parser SignalPair
parseSignalPair = do
    left <- parseSignal
    P.endOfLine
    right <- parseSignal
    P.endOfLine
    return (left, right)

parseSignal :: P.Parser Signal
parseSignal = P.between (P.char '[') (P.char ']') (P.sepBy parsePacket (P.char ','))

parsePacket :: P.Parser Packet
parsePacket = Multiple <$> parseSignal <|> parseSingle

parseSingle :: P.Parser Packet
parseSingle = Single . read <$> P.many1 P.digit