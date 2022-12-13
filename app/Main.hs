module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad

import Options.Applicative
import Data.Semigroup ((<>))

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

runDay :: Options -> IO ()
runDay (Options 1) = run Calories.runInput "input/Day1.txt"
runDay (Options 2) = run RockPaperScissors.runInput "input/Day2.txt"
runDay (Options 3) = run Backpack.runInput "input/Day3.txt"
runDay (Options 4) = run CampCleanup.runInput "input/Day4.txt"
runDay (Options 5) = run SupplyStacks.runInput "input/Day5.txt"
runDay (Options 6) = run TuningTrouble.runInput "input/Day6.txt"
runDay (Options 7) = run NoSpaceLeft.runInput "input/Day7.txt"
runDay (Options 8) = run TreetopTreeHouse.runInput "input/Day8.txt"
runDay (Options 9) = run RopeBridge.runInput "input/Day9.txt"
runDay (Options 10) = run CathodeRayTube.runInput "input/Day10.txt"
runDay (Options 11) = run MonkeyInTheMiddle.runInput "input/Day11.txt"
runDay (Options 12) = run HillClimbingAlgorithm.runInput "input/Day12.txt"
runDay (Options 13) = run DistressSignal.runInput "input/Day13.txt"

runDay _ = return ()

run :: Show a => (T.Text -> a) -> FilePath -> IO ()
run action file = do
    result <- action <$> T.readFile file
    putStrLn $ show result
