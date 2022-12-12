module HillClimbingAlgorithm where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Foldable (foldl')
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P
import Text.Parsec ((<|>))
import Data.Maybe (mapMaybe)
import PrettyPrint (printKeyValuePairs)
import Debug.Trace (trace)
import qualified Data.Char as C
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.SP as G
import qualified Data.Graph.Inductive.Query.BFS as G

type Elevation = Int
data ElevationKind = Regular | Start | End deriving Show

type NodeData = (Int, Int, Elevation, ElevationKind) 

data Result = Result {
    pt1 :: Int,
    pt2 :: Int
} 

instance Show Result where    
    show (Result {..}) = printKeyValuePairs [
        ("Shortest Path from Start to End" :: T.Text, show pt1),
        ("Shortest Path from any 'a' to End" :: T.Text, show pt2)]

-- Entry point
runInput :: T.Text -> Result
runInput input = solve $ parseInput input

-- Logic
solve :: G.Gr NodeData Int -> Result
solve graph = Result (solvePt1 graph) (solvePt2 graph)
          
solvePt1 :: G.Gr NodeData Int -> Int
solvePt1 graph = L.length shortestPath - 1
        where nodes = G.labNodes graph
              (startNode,_) = head $ filter isStart nodes
              (endNode, _) = head $ filter isEnd nodes
              (Just shortestPath) = G.sp startNode endNode graph

solvePt2 :: G.Gr NodeData Int -> Int
solvePt2 graph = L.length shortestPath - 1
        where nodes = G.labNodes graph
              (endNode, _) = head $ filter isEnd nodes
              inverseEdges = inverseEdge <$> G.labEdges graph
              (graph' :: G.Gr NodeData Int) = G.mkGraph nodes inverseEdges
              paths = G.bft endNode graph'
              shortestPath = head $ filter (isPathToA graph) paths

isPathToA :: G.Gr NodeData Int -> G.Path -> Bool 
isPathToA graph (last : _) = case G.lab graph last of 
    Just (_, _, 0, _) -> True
    _ -> False
                
inverseEdge :: G.LEdge Int -> G.LEdge Int
inverseEdge (s, d, l) = (d, s, l)

isStart :: G.LNode NodeData -> Bool
isStart (node, (_, _,_, Start)) = True
isStart _ = False

isEnd :: G.LNode NodeData  -> Bool
isEnd (node, (_, _,_, End)) = True
isEnd _ = False
    
-- Parsing
parseInput :: T.Text -> G.Gr NodeData Int
parseInput lines = case eitherCommands of 
            Right commands -> commands
            Left err -> error $ show err
    where parser = do 
            input <- (P.sepEndBy1 (P.many1 parseElevation) P.endOfLine)
            let nodes = toNodes input
            let edges = toEdges nodes <> toEdges (L.transpose nodes)
            return $ G.mkGraph (concat nodes) edges
          (eitherCommands) = P.parse parser "input" lines

toNodes :: [[(Elevation, ElevationKind)]] -> [[G.LNode NodeData]]
toNodes input = do 
        (y, row) <- zip [0..] input
        return $ do 
            (x, (elevation, elevationKind)) <- zip [0..] row
            return $ toLNode y x (length row) elevation elevationKind

toEdges :: [[G.LNode NodeData]] -> [G.LEdge Int]
toEdges nodes = do 
    nodeRow <- nodes
    ((lid, (_, _, el, _)), (rid, (_, _, er, _))) <- zip nodeRow (tail nodeRow) 
    let lToR = if er - el <= 1 then [(lid,rid, 1)] else []
    let lToL = if el - er <= 1 then [(rid, lid, 1)] else []
    lToR <> lToL

toLNode :: Int -> Int -> Int -> Elevation -> ElevationKind -> G.LNode NodeData
toLNode y x xs elevation elevationKind = (y * xs + x, (x, y, elevation, elevationKind))

parseElevation :: P.Parser (Elevation, ElevationKind)
parseElevation = inspect <$> P.letter 
    where inspect 'S' = (toElevation 'a', Start)
          inspect 'E' = (toElevation 'z', End)
          inspect it = (toElevation it, Regular)
          toElevation it = C.ord it - C.ord 'a'