module SupplyStacks where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Set as S
import Data.Foldable (foldl')
import Prettyprinter hiding (group, space)
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P
import Data.Maybe (catMaybes)
import Data.Char (digitToInt)

type Crate = Char
type SupplyStack = [Crate]
data Move = Move {
    count :: Int,
    source :: Int,
    destination :: Int
} deriving (Show)

data Result = Result {
    topCrates9000 :: [Crate],
    topCrates9001 :: [Crate]
} 

-- Result pretty printing
instance Show Result where    
    show (Result {..}) = show . vcat $ (\(key, value) -> fillBreak ((maximum $ T.length . fst <$> kvs) + 3) (pretty key <> ":") <+> pretty value) <$> kvs
        where kvs = [("Top Crates 9000" :: T.Text, show topCrates9000), ("Top Crates 9001", show topCrates9001)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseInput input

-- Logic
solve :: ([SupplyStack], [Move]) -> Result
solve (supplyStacks, moves) = Result topCrates9000 topCrates9001
    where topCrates9000 = head <$> applyMoves L.reverse supplyStacks moves
          topCrates9001 = head <$> applyMoves id supplyStacks moves

applyMoves :: (SupplyStack -> SupplyStack)  -> [SupplyStack] -> [Move] -> [SupplyStack]
applyMoves moveFn supplyStacks moves = foldl' (applyMove moveFn) supplyStacks moves

applyMove :: (SupplyStack -> SupplyStack) -> [SupplyStack] -> Move -> [SupplyStack]
applyMove moveFn supplyStacks (Move count source destination) = updateStack <$> indexedStacks 
    where indexedStacks = zip [1..] supplyStacks
          crates = moveFn . take count . snd <$> head $ L.filter (\(i, _) -> i == source) indexedStacks 
          updateStack :: (Int, SupplyStack) -> SupplyStack
          updateStack (idx, stack) | idx == source = drop count stack
          updateStack (idx, stack) | idx == destination = crates <> stack
          updateStack (idx, stack) = stack

-- Parsing
parseInput :: T.Text -> ([SupplyStack], [Move])
parseInput lines = result
    where parser = do
            supplyStacks <- parseSupplyStacks  
            P.count 2 $ P.manyTill P.anyChar P.endOfLine
            moves <- P.sepEndBy1 parseMove P.endOfLine
            return $ (supplyStacks, moves)
          (Right result) = P.parse parser "input" lines

parseMove :: P.Parser Move
parseMove = do
        P.string "move" <* space
        count <- read <$> P.many1 P.digit <* space        
        P.string "from" <* space        
        source <- read <$> P.many1 P.digit <* space
        P.string "to" <* space
        destination <- read <$> P.many1 P.digit
        return $ Move count source destination


parseSupplyStacks :: P.Parser [SupplyStack]
parseSupplyStacks = do
        stacks <- P.manyTill (parsePositions <* P.endOfLine) space
        return $ catMaybes <$> L.transpose stacks

parsePositions :: P.Parser [Maybe Crate]
parsePositions = P.sepBy1 parsePosition space

parsePosition ::  P.Parser (Maybe Crate)
parsePosition = (Just <$> parseCrate) P.<|> ((\_ -> Nothing) <$> parseEmpty)

parseCrate :: P.Parser Crate
parseCrate = P.between (P.char '[') (P.char ']') P.upper

parseEmpty :: P.Parser ()
parseEmpty = (\_ -> ()) <$> P.count 3 space

space :: P.Parser ()
space = (\_ -> ()) <$> P.char ' '