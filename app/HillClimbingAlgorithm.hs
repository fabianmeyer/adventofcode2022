module HillClimbingAlgorithm where

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

data Command = Noop | AddX Int deriving (Show)

data CommandQueueEntry = CommandQueueEntry Int Command deriving (Show)

data Result = Result {
    sumInterestedCycles :: Int,
    image :: String
} 

data State = State [CommandQueueEntry] Int

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("Sum Interested Cycles" :: T.Text, show sumInterestedCycles),
        ("Image" :: T.Text, "\n" <> image)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseInput input

-- Logic
solve :: [Command] -> Result
solve commands = Result (sum interestedCycles) image
    where commandQueueEntries = mkCommandQueueEntry <$> commands
          initial = State commandQueueEntries 1
          steps = zip [1..] $ run initial
          xs = (\(cycle, (State _ x)) -> (cycle, x)) <$> steps
          interestedCycles = (\(cycle, x) -> cycle * x) . head <$> (L.chunksOf 40 $ drop 19 xs)
          image = unlines $ map draw <$> (L.chunksOf 40 xs)

draw :: (Int, Int) -> Char
draw (c, x) | abs (((c - 1) `mod` 40) - x) < 2 = '#'
draw _ = '.'

mkCommandQueueEntry :: Command -> CommandQueueEntry
mkCommandQueueEntry Noop = CommandQueueEntry 1 Noop
mkCommandQueueEntry cmd@(AddX _) = CommandQueueEntry 2 cmd

run :: State -> [State]
run s@(State ((CommandQueueEntry 1 Noop) : tail) x) = s : run (State tail x)
run s@(State ((CommandQueueEntry 1 (AddX dx)) : tail) x) = s : run (State tail (x + dx))
run s@(State ((CommandQueueEntry n ce) : tail) x) = s : run (State (CommandQueueEntry (n - 1) ce : tail) x)
run (State [] _) = []

-- Parsing
parseInput :: T.Text -> [Command]
parseInput lines = case eitherCommands of 
            Right commands -> commands
            Left err -> error $ show err
    where parser = (P.sepEndBy1 parseCommand P.endOfLine)
          (eitherCommands) = P.parse parser "input" lines

parseCommand :: P.Parser Command  
parseCommand = parseNoop <|> parseAddX

parseNoop :: P.Parser Command  
parseNoop = P.string "noop" *> return Noop

parseAddX :: P.Parser Command  
parseAddX = do 
    P.string "addx" *> P.space 
    prefix <- P.option "" $ P.string "-"
    number <- P.many1 P.digit
    return . AddX . read $ prefix <> number


-- parseCommand :: P.Parser Command
-- parseCommand = P.string "$" *> P.space *> (parseChangeDirectory <|> parseListContent)
    
-- parseChangeDirectory ::  P.Parser Command
-- parseChangeDirectory = do
--         P.string "cd" <* P.space
--         path <- parsePath
--         return $ ChangeDirectory path

-- parsePath :: P.Parser Path
-- parsePath = parseParent <|> parseRoot <|> parseSubDirectory

-- parseParent :: P.Parser Path
-- parseParent = P.string ".." *> return Parent

-- parseRoot :: P.Parser Path
-- parseRoot = P.string "/" *> return Root

-- parseSubDirectory :: P.Parser Path
-- parseSubDirectory = SubDirectory <$> parseName

-- parseListContent ::  P.Parser Command
-- parseListContent = P.string "ls" *> pure ListContent
        
-- parseDirectoryContent :: P.Parser DirectoryContent
-- parseDirectoryContent = parseDirectory <|> parseFile

-- parseDirectory :: P.Parser DirectoryContent
-- parseDirectory = Directory <$> (P.string "dir" *> P.space *> parseName)

-- parseFile :: P.Parser DirectoryContent
-- parseFile = do
--     size <- read <$> P.many1 P.digit <* P.space
--     name <- parseName
--     return $ File name size

-- parseName :: P.Parser T.Text
-- parseName = T.pack <$> P.many1 (P.satisfy (/= '\n'))
