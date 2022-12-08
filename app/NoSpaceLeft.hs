module NoSpaceLeft where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.Foldable (foldl')
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P
import Text.Parsec ((<|>))
import Data.Maybe (catMaybes)
import PrettyPrint


data Path = SubDirectory T.Text | Parent  | Root deriving (Show)
data Command = ChangeDirectory Path | ListContent deriving (Show)
data DirectoryContent = Directory T.Text | File T.Text Integer deriving (Show)
data Output = CommandOutput Command | DirectoryContentOutput DirectoryContent deriving (Show)
data AbsoluteFile = AbsoluteFile {directory :: [T.Text], name :: T.Text, size :: Integer} deriving (Show) 

data Result = Result {
    sumSizeSmallFolders :: Integer,
    directoryToDelete :: ([T.Text], Integer)
} 

totalDiskSpace :: Integer
totalDiskSpace = 70000000

requiredUnusedSpace :: Integer
requiredUnusedSpace = 30000000

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("Sum Size Small Folders" :: T.Text, show sumSizeSmallFolders),
        ("Directory to Delete" :: T.Text, show directoryToDelete)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseInput input

-- Logic
solve :: [Output] -> Result
solve commands = Result sumSizeSmallFolders directoryToDelete
    where absoluteFiles = mkAbsolute [] commands
          dirSizes = directorySizes absoluteFiles
          sortedDirSizes = L.sortOn snd dirSizes
          sumSizeSmallFolders = sum $ snd <$> takeWhile (((<= 100000) . snd)) sortedDirSizes
          usedDiskSpace = snd . last $ sortedDirSizes
          diskSpaceToFree = usedDiskSpace - (totalDiskSpace - requiredUnusedSpace)
          directoryToDelete = head $ dropWhile (((<= diskSpaceToFree) . snd)) sortedDirSizes

mkAbsolute :: [T.Text] -> [Output] -> [AbsoluteFile]
mkAbsolute path (CommandOutput (ChangeDirectory Root) : tail) = mkAbsolute [] tail
mkAbsolute path (CommandOutput (ChangeDirectory (SubDirectory dir)) : tail) = mkAbsolute (dir : path) tail
mkAbsolute (_ : path) (CommandOutput (ChangeDirectory Parent) : tail) = mkAbsolute path tail
mkAbsolute path (DirectoryContentOutput (File name size) : tail) = AbsoluteFile (reverse path) name size : mkAbsolute path tail
mkAbsolute path (_ : tail) = mkAbsolute path tail
mkAbsolute _ [] = []

directorySizes :: [AbsoluteFile] -> [([T.Text], Integer)]
directorySizes files = absoluteDirectorySizes
    where sortedFiles = L.sortOn directory files 
          groupedFiles = L.groupOn directory sortedFiles
          dirSize files@(AbsoluteFile dir _ _ : _) = (dir, sum $ size <$> files)
          dirSizes = dirSize <$> groupedFiles
          directories =  L.nub $ dirSizes >>= allDirectories . fst
          absoluteDirectorySizes = (\dir -> (dir, sum $ snd <$> filter (\(p, _) -> dir == L.take (L.length dir) p) dirSizes)) <$> directories

allDirectories :: [T.Text] -> [[T.Text]]
allDirectories [] = [[]]
allDirectories it = it : allDirectories (init it)

-- Parsing
parseInput :: T.Text -> [Output]
parseInput lines = case eitherCommands of 
            Right commands -> commands
            Left err -> error $ show err
    where parser = (P.sepEndBy1 parseOutput P.endOfLine)
          (eitherCommands) = P.parse parser "input" lines

parseOutput :: P.Parser Output          
parseOutput = CommandOutput <$> parseCommand <|> DirectoryContentOutput <$> parseDirectoryContent

parseCommand :: P.Parser Command
parseCommand = P.string "$" *> P.space *> (parseChangeDirectory <|> parseListContent)
    
parseChangeDirectory ::  P.Parser Command
parseChangeDirectory = do
        P.string "cd" <* P.space
        path <- parsePath
        return $ ChangeDirectory path

parsePath :: P.Parser Path
parsePath = parseParent <|> parseRoot <|> parseSubDirectory

parseParent :: P.Parser Path
parseParent = P.string ".." *> return Parent

parseRoot :: P.Parser Path
parseRoot = P.string "/" *> return Root

parseSubDirectory :: P.Parser Path
parseSubDirectory = SubDirectory <$> parseName

parseListContent ::  P.Parser Command
parseListContent = P.string "ls" *> pure ListContent
        
parseDirectoryContent :: P.Parser DirectoryContent
parseDirectoryContent = parseDirectory <|> parseFile

parseDirectory :: P.Parser DirectoryContent
parseDirectory = Directory <$> (P.string "dir" *> P.space *> parseName)

parseFile :: P.Parser DirectoryContent
parseFile = do
    size <- read <$> P.many1 P.digit <* P.space
    name <- parseName
    return $ File name size

parseName :: P.Parser T.Text
parseName = T.pack <$> P.many1 (P.satisfy (/= '\n'))