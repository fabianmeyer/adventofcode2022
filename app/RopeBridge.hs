module RopeBridge where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import PrettyPrint
import Debug.Trace (trace)

data Vector = Vector Int Int deriving (Show, Eq)

data Result = Result {
    distinctTailHeadPositionCount :: Int,
    distinctTailPositionCount :: Int
} 

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("Tail Head Position Count" :: T.Text, show distinctTailHeadPositionCount),
        ("Tail Tail Position Count" :: T.Text, show distinctTailPositionCount)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseInput input

-- Logic
solve :: [Vector] -> Result
solve moves = Result distinctTailHeadPositionCount distinctTailPositionCount
    where unitMoves = moves >>= unitVectors
          unitPositions = scanl move (take 10 . repeat $ Vector 0 0) unitMoves
          tailPositions = tail <$> unitPositions
          distinctTailHeadPositionCount = L.length . L.nub $ head <$> tailPositions
          distinctTailPositionCount = L.length . L.nub $ last <$> tailPositions


unitVectors :: Vector -> [Vector]
unitVectors (Vector x y) = (if x >= 0 then take x $ repeat $ Vector 1 0 else take (abs x) $ repeat $ Vector (-1) 0)
                        <> (if y >= 0 then take y $ repeat $ Vector 0 1 else take (abs y) $ repeat $ Vector 0 (-1))

move :: [Vector] -> Vector -> [Vector] 
move (h : t) v = moveTail (h' : t)
    where h' = moveHead h v

moveHead :: Vector -> Vector -> Vector
moveHead (Vector x y) (Vector dx dy) = Vector (x + dx) (y + dy)

moveTail :: [Vector] -> [Vector]
moveTail (h@(Vector hx hy) : (t@(Vector tx ty) : tail)) = h : moveTail (t' : tail)
    where t' = case ((hx - tx), (hy - ty)) of 
            (dx, dy) | abs dx < 2 && abs dy < 2 -> t
            (dx, 0) | dx > 0 -> Vector (tx + 1) ty
            (dx, 0) | dx < 0 -> Vector (tx - 1) ty
            (0, dy) | dy > 0 -> Vector tx (ty + 1)
            (0, dy) | dy < 0 -> Vector tx (ty - 1)
            (dx, dy) | dx > 0 && dy > 0  -> Vector (tx + 1) (ty + 1)
            (dx, dy) | dx > 0 && dy < 0  -> Vector (tx + 1) (ty - 1)
            (dx, dy) | dx < 0 && dy > 0  -> Vector (tx - 1) (ty + 1)
            (dx, dy) | dx < 0 && dy < 0  -> Vector (tx - 1) (ty - 1)
            _ -> t
moveTail t = t


-- Parsing
parseInput :: T.Text -> [Vector]
parseInput lines = parseMove <$> T.lines lines

parseMove :: T.Text -> Vector
parseMove line = (case dir of 
        "L" -> Vector (-length) 0
        "R" -> Vector length 0
        "U" -> Vector 0 length
        _ -> Vector 0 (-length))
    where [dir, lengthStr] = T.splitOn " " line
          length = read . T.unpack $ lengthStr
