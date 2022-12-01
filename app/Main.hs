module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad

import qualified Calories as Calories

main :: IO ()
main = do
    day1Result <- Calories.runInput <$> T.readFile "input/Day1.txt"
    forM_ day1Result (putStrLn . T.unpack) 
