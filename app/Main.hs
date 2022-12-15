module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Map as M
import Data.Map (Map)
import Data.Foldable (traverse_)

import qualified Calories
import qualified RockPaperScissors
import qualified Backpack
import qualified CampCleanup
import qualified SupplyStacks
import qualified TuningTrouble
import qualified NoSpaceLeft
import qualified TreetopTreeHouse
import qualified RopeBridge
import qualified CathodeRayTube
import qualified MonkeyInTheMiddle
import qualified HillClimbingAlgorithm
import qualified DistressSignal
import qualified RegolithReservoir

data Options = Options
  { day :: Int }

main :: IO ()
main = runDay =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Run solutions of Advent of Code 2022"
     <> header "Advent of Code 2022" )

sample :: Parser Options
sample = Options <$> argument auto (metavar "DAY" <> help "Day to run" )

options :: Map Int (T.Text -> String)
options = M.fromList [
    (1, show . Calories.runInput)
  , (2, show . RockPaperScissors.runInput)
  , (3, show . Backpack.runInput)
  , (4, show . CampCleanup.runInput)
  , (5, show . SupplyStacks.runInput)
  , (6, show . TuningTrouble.runInput)
  , (7, show . NoSpaceLeft.runInput)
  , (8, show . TreetopTreeHouse.runInput)
  , (9, show . RopeBridge.runInput)
  , (10, show . CathodeRayTube.runInput)
  , (11, show . MonkeyInTheMiddle.runInput)
  , (12, show . HillClimbingAlgorithm.runInput)
  , (13, show . DistressSignal.runInput)
  , (14, show . RegolithReservoir.runInput)]

runDay :: Options -> IO ()
runDay (Options opt) = result  
  where maybeF = M.lookup opt options
        result = traverse_ (\f -> run f ("input/Day" <> show opt <> ".txt")) maybeF


run :: (T.Text -> String) -> FilePath -> IO ()
run action file = do
    result <- action <$> T.readFile file
    putStrLn $ result
