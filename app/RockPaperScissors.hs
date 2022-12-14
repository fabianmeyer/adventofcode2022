module RockPaperScissors where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List as L
import qualified Data.List.Split as L
import PrettyPrint

-- Data Types
data RPC = Rock | Paper | Scissors deriving (Show, Eq)
data Variable = X | Y | Z  deriving (Show)

data Round a = Round {
    opponent :: RPC,
    me :: a
}  deriving (Show)

type Tournament a = [Round a]

data Result = Result {
    firstScore :: Integer,
    secondScore :: Integer
} 

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("First Score" :: T.Text, show firstScore), 
        ("Second Score", show secondScore)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseTournament input

solve :: Tournament Variable -> Result
solve freeTournament = Result firstScore secondScore
        where firstScore = scoreTournament $ bindTournament firstBinding freeTournament
              secondScore = scoreTournament $ bindTournament secondBinding freeTournament

-- Binding
bindTournament :: (Round a -> Round RPC) -> Tournament a -> Tournament RPC
bindTournament = map

firstBinding :: Round Variable -> Round RPC
firstBinding (Round o X) = Round o Rock
firstBinding (Round o Y) = Round o Paper
firstBinding (Round o Z) = Round o Scissors

secondBinding :: Round Variable -> Round RPC
secondBinding (Round Rock X) = Round Rock Scissors
secondBinding (Round Rock Z) = Round Rock Paper
secondBinding (Round Paper X) = Round Paper Rock
secondBinding (Round Paper Z) = Round Paper Scissors
secondBinding (Round Scissors X) = Round Scissors Paper
secondBinding (Round Scissors Z) = Round Scissors Rock
secondBinding (Round o _) = Round o o

-- Scoring
rpcScore :: RPC -> Integer
rpcScore Rock = 1
rpcScore Paper = 2
rpcScore Scissors = 3

roundScore :: RPC -> RPC -> Integer
roundScore Rock Scissors = 6
roundScore Paper Rock = 6
roundScore Scissors Paper = 6
roundScore fst snd | fst == snd = 3
roundScore _ _ = 0

scoreRound :: Round RPC -> Integer
scoreRound (Round fst snd) = roundScore snd fst + rpcScore snd

scoreTournament :: Tournament RPC -> Integer
scoreTournament rounds = sum $ scoreRound <$> rounds

-- Parsing
parseRPC :: T.Text -> RPC
parseRPC "A" = Rock
parseRPC "B" = Paper
parseRPC "C" = Scissors
parseRPC it = error $ "Error parsing RPC from " <> T.unpack it

parseVariable :: T.Text -> Variable
parseVariable "X" = X
parseVariable "Y" = Y
parseVariable "Z" = Z
parseVariable it = error $ "Error parsing variable from " <> T.unpack it

parseRound :: T.Text -> Round Variable
parseRound line = Round (parseRPC fst) (parseVariable snd)
    where [fst, snd] = T.splitOn " " line

parseTournament :: T.Text -> Tournament Variable
parseTournament input = parseRound <$> T.lines input
