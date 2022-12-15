module RegolithReservoir where

import qualified Data.Text as T
import PrettyPrint (printKeyValuePairs)
import Debug.Trace (trace)
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P
import qualified Data.List as L
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.SP as G
import qualified Data.Graph.Inductive.Query.BFS as G
import Data.Maybe (fromJust)

data Point = Point {
    x :: Int,
    y :: Int
 } deriving (Show, Eq)
type Path = [Point]

data Result = Result {
    pt1 :: Int,
    pt2 :: Int
} 

data Direction = D | L | R deriving (Show, Eq)
instance Ord Direction where
    D <= L = True
    D <= R = True
    L <= R = True
    _ <= _ = False

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("Sand resting on rocks" :: T.Text, show pt1),
        ("Sand blocking pouring hole" :: T.Text, show pt2)
        ]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseInput input

-- Logic
pouringPoint = Point 500 0

solve :: [Path] -> Result
solve rockPaths = Result abyssIdx pouringIdx
    where rockSubPaths = rockPaths >>= subPaths
          rocks = rockSubPaths >>= uncurry points
          (ymin, ymax) = (0, (maximum $ y <$> rocks) + 1)
          (xmin, xmax) = (x pouringPoint - ymax - 10, x pouringPoint + ymax + 10)
          nodes = (\y -> (\x -> toNode xmax $ Point x y) <$> [xmin .. xmax] ) <$> [ymin .. ymax]                   
          edges' = edges nodes  
          (world :: G.Gr Point Direction) = G.mkGraph (concat nodes) edges'
          world' = G.delNodes (fst . toNode xmax <$> rocks) $ world
          (pouringNode, _) = toNode xmax pouringPoint

          finalLocations' = (zip [0..] $ iterate (\old@(w, _) -> pour pouringNode old) (world', Nothing)) >>= (\(idx, (w, n)) -> case n of 
                Just p -> [(idx, w, p)]
                _ -> [])

          finalLocations'' = dropWhile (\(_, _, (Point _ y)) -> y < ymax) $ finalLocations'
          (abyssIdx, _, _) = head finalLocations''
          (pouringIdx, _, _) = head . dropWhile (\(_, _, p) -> pouringPoint /= p) $ finalLocations''



draw :: [Point] -> Point -> Int -> Int -> Int -> Int -> G.Gr Point Direction -> String
draw rocks pouringPoint xmin xmax ymin ymax graph = unlines points
    where points = (\y -> ((\x -> drawPoint $ Point x y) <$> [xmin .. xmax]) <> " " <> show y ) <$> [ymin .. ymax]
          drawPoint :: Point -> Char
          drawPoint point | point `elem` rocks = '#'
          drawPoint point | point == pouringPoint = '+'
          drawPoint point = case G.lab graph (nodeId xmax point) of
                Just _ -> '.'
                _ -> 'o'

pour :: G.Node -> (G.Gr Point Direction, Maybe Point) -> (G.Gr Point Direction, Maybe Point) 
pour p (g, _) = (G.delNode finalLocation' g, finalLocationLabel)
    where finalLocation' = finalLocation g p
          finalLocationLabel = G.lab g finalLocation'

finalLocation :: G.Gr Point Direction -> G.Node ->  G.Node 
finalLocation g s = case (L.sortOn (\(_,_, dir) -> dir) $ G.out g s) of
        (_, dest, _) : _ -> finalLocation g dest
        [] -> s

toNode :: Int -> Point -> G.LNode Point
toNode xmax p = (nodeId xmax p, p)

nodeId :: Int -> Point -> Int
nodeId xmax (Point x y) = y * (xmax + 1) + x

subPaths :: Path -> [(Point, Point)]
subPaths path = zip path $ tail path

points :: Point -> Point -> [Point]
points (Point ax ay) (Point bx by) | ay == by = (\x -> Point x ay) <$> [(min ax bx) .. (max ax bx)]
points (Point ax ay) (Point bx by) | ax == bx = (\y -> Point ax y) <$> [(min ay by) .. (max ay by)]

edges :: [[(Int, Point)]] -> [G.LEdge Direction]
edges (a : t@(b : _)) = left <> down <> right <> edges t
    where left = zip3 (fst <$> tail a) (fst <$> b) (repeat L)
          down = zip3 (fst <$> a) (fst <$> b) (repeat D)
          right = zip3 (fst <$> a) (fst <$> tail b) (repeat R)
edges _ = []

-- Parsing
parseInput :: T.Text -> [Path]
parseInput lines = case eitherCommands of 
            Right paths -> paths
            Left err -> error $ show err
    where (eitherCommands) = P.parse parsePaths "input" lines

parsePaths :: P.Parser [Path]
parsePaths = P.sepEndBy1 parsePath P.endOfLine

parsePath :: P.Parser Path
parsePath = P.sepBy1 parsePoint (P.string " -> ")

parsePoint :: P.Parser Point
parsePoint = do 
    x <- read <$> P.many1 P.digit
    P.char ','
    y <- read <$> P.many1 P.digit
    return $ Point x y