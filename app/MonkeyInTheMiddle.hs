module MonkeyInTheMiddle where

import Prelude hiding (round)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Foldable (foldl')
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P
import Text.Parsec ((<|>))
import Data.Maybe (catMaybes)
import PrettyPrint
import Debug.Trace (trace)

type MonkeyId = Int
type Item = Integer

data Monkey = Monkey {
  monkeyId :: !MonkeyId,
  inspections :: !Integer,
  inspectionParams :: !Expression,
  testParams :: !Test,
  items :: ![Item]
} deriving (Show)
data Test = Test {
    divisor :: Integer,
    trueMonkey :: MonkeyId,
    falseMonkey :: MonkeyId
 } deriving (Show)

data Expression = Add Operand Operand | Mult Operand Operand  deriving (Show)
data Operand = Old | Literal Integer deriving (Show)

data Result = Result {
    monkeyBusinessPt1 :: Integer,
    monkeyBusinessPt2 :: Integer
} 

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("Monkey Business Pt1" :: T.Text, show monkeyBusinessPt1),
        ("Monkey Business Pt2" :: T.Text, show monkeyBusinessPt2)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseInput input

-- Logic
solve :: [Monkey] -> Result
solve monkeys = Result monkeyBusinessPt1 monkeyBusinessPt2
    where monkeyBusinessPt1 = solvePt1 monkeys
          monkeyBusinessPt2 = solvePt2 monkeys

solvePt1 :: [Monkey] -> Integer
solvePt1 monkeys = monkeyBusinessPt1
        where 
          roundsPt1 = L.iterate' (round (\i -> (i `div` 3))) monkeys
          roundPt1 = roundsPt1 !! 20
          monkeyBusinessPt1 = monkeyBusiness roundPt1
          
solvePt2 :: [Monkey] -> Integer
solvePt2 monkeys = monkeyBusinessPt2
        where
          kgv = product $ divisor . testParams <$> monkeys
          roundsPt2 = L.iterate' (round (\it -> it `mod` kgv)) monkeys 
          roundPt2 = roundsPt2 !! 10000
          monkeyBusinessPt2 = monkeyBusiness roundPt2


monkeyBusiness :: [Monkey] -> Integer
monkeyBusiness monkeys = inspections fst * inspections snd
    where fst : snd : _ = L.sortOn ((*) (-1) . inspections) monkeys

round :: (Integer -> Integer) -> [Monkey] -> [Monkey]
round relief monkeys = L.iterate' (turn relief) monkeys !! L.length monkeys

turn :: (Integer -> Integer) -> [Monkey] -> [Monkey]
turn relief (Monkey monkeyId inspections exp test items : monkeys) = monkeys'
    where thrownItems = processItem relief exp test <$> items
          monkeys' = catch thrownItems <$> monkeys <> [Monkey monkeyId (inspections + fromIntegral (length thrownItems)) exp test []]

catch :: [(MonkeyId, Item)] -> Monkey -> Monkey
catch thrownItems (Monkey monkeyId inspections exp test items) = (Monkey monkeyId inspections exp test items')
    where !items' = items <> (snd <$> filter ((== monkeyId) . fst) thrownItems)

processItem :: (Integer -> Integer) -> Expression -> Test -> Item -> (MonkeyId, Item)
processItem relief exp tst = test tst . relief . inspect exp

inspect :: Expression -> Item -> Item
inspect (Add lhs rhs) item = value lhs item + value rhs item
inspect (Mult lhs rhs) item = value lhs item * value rhs item

value :: Operand -> Item -> Item
value Old item = item
value (Literal lit) _ = lit


test :: Test -> Item -> (MonkeyId, Item)
test (Test divisor t _) item | item `mod` divisor == 0 = (t, item)
test (Test _ _ f) item = (f, item)

-- Parsing
parseInput :: T.Text -> [Monkey]
parseInput lines = case eitherCommands of 
            Right commands -> commands
            Left err -> error $ show err
    where parser = (P.sepEndBy1 parseMonkey P.endOfLine)
          (eitherCommands) = P.parse parser "input" lines

parseMonkey :: P.Parser Monkey  
parseMonkey = do
    monkeyId <- read <$> (P.string "Monkey" *> P.space *> P.many1 P.digit <* P.string ":" <* P.endOfLine)
    items <- map read <$> (P.string "  Starting items: " *> P.sepBy1 (P.many1 P.digit) (P.string ", ")) <* P.endOfLine
    operation <- P.string "  Operation: new = " *> parseExpression <* P.endOfLine
    test <- parseTest <* P.endOfLine
    return $ Monkey monkeyId 0 operation test items

parseExpression :: P.Parser Expression
parseExpression = do
        lhs <- parseOld <|> praseLit
        P.space
        op <- P.char '+' <|> P.char '*'
        P.space
        rhs <- parseOld <|> praseLit
        return $ case op of 
            '+' -> Add lhs rhs
            '*' -> Mult lhs rhs
    where parseOld = P.string "old" *> return Old
          praseLit = Literal . read <$> P.many1 P.digit
          
parseTest :: P.Parser Test
parseTest = do
    divisor <- read <$> (P.string "  Test: divisible by " *> P.many1 P.digit <* P.endOfLine)
    trueCase <- read <$> (P.string "    If true: throw to monkey " *> P.many1 P.digit <* P.endOfLine)
    falseCase <- read <$> (P.string "    If false: throw to monkey " *> P.many1 P.digit)
    return $ Test divisor trueCase falseCase