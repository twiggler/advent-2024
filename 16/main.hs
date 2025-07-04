import AxisAligned (Coord2)
import Control.Monad (join)
import Cursor (CardinalDir (East), toDir, toVector2)
import Data.Array (Array, (!))
import Data.Array qualified as A (assocs, listArray)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Functor (($>))
import Data.List qualified as L (filter, find, length, transpose, zip, foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M (empty, findWithDefault, insert, lookup, notMember)
import Data.Maybe (mapMaybe)
import Data.OrdPSQ qualified as PQ (alter, minView, singleton)
import Parsing (grid, parseFileWith)
import Safe (minimumMay)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof)

data Cell = Empty | Wall | Start | End deriving (Show, Eq, Ord)

data MazeNode = MazeNode
  { pos :: Coord2,
    dir :: CardinalDir -- Travel cost depends on direction
  }
  deriving (Show, Eq, Ord)

data SptValue = SptValue
  { cost :: Int, -- Cost to reach this node
    preds :: [MazeNode] -- Predecessors in the shortest path tree
  }
  deriving (Show, Eq)

type MazeArray = Array Coord2 Cell

cell :: ReadP Cell
cell = choice [char '.' $> Empty, char '#' $> Wall, char 'S' $> Start, char 'E' $> End]

neighbors :: Array Coord2 Cell -> Coord2 -> [(Coord2, Cell, CardinalDir)]
neighbors maze (cx, cy) =
  [ (p, maze ! p, dir')
    | dir' <- [minBound .. maxBound],
      let (dx, dy) = (toVector2 . toDir) dir',
      let p = (cx + dx, cy + dy)
  ]

travelCost :: MazeNode -> MazeNode -> Int
travelCost source dest
  | dx1 == dx2 && dy1 == dy2 = 1 -- Same direction
  | dx1 == -dx2 || dy1 == -dy2 = 1000 * 2 + 1 -- Reverse direction
  | otherwise = 1000 + 1 -- 90 degree turn
  where
    (dx1, dy1) = (toVector2 . toDir . dir) source
    (dx2, dy2) = (toVector2 . toDir . dir) dest

next :: MazeArray -> MazeNode -> [MazeNode]
next maze (MazeNode pos' _) =
  [MazeNode {pos = p, dir = dir'} | (p, cell', dir') <- neighbors maze pos', cell' /= Wall]

-- Suitable for a shortest paths tree (no cycles)
dfs :: Map MazeNode [MazeNode] -> [MazeNode] -> [MazeNode]
dfs maze = go
  where
    go [] = []
    go (s : rest) = s : go (rest ++ M.findWithDefault [] s maze)

-- Dijkstra's algorithm with a twist: we return a tree of all the shortest paths.
-- Can be turned into library by injecting next and travelCost functions.  
dijkstra :: MazeArray -> MazeNode -> Map MazeNode SptValue
dijkstra maze source =
  go (PQ.singleton source 0 []) M.empty -- Fill the priority queue as we explore the maze 
  where
    go queue spt =
      case PQ.minView queue of
        Nothing -> spt
        Just (node, cost', preds', queue') ->
          let spt' = M.insert node SptValue {cost = cost', preds = preds'} spt
              nextAssocCosts = nextWithCosts node cost'
              updatedQueue = L.foldl' (updateQueue node) queue' nextAssocCosts
           in go updatedQueue spt'
      where
        nextWithCosts source' cumCost =
          let nextStates = L.filter (`M.notMember` spt) (next maze source')
              costs = (\target -> cumCost + travelCost source' target) <$> nextStates
           in (nextStates `L.zip` costs)

        updateQueue source' queue' (dest, cost') =
          snd $ PQ.alter (maybe ((), Just (cost', [source'])) upd') dest queue'
          where
            upd (oldCost, oldPreds)
              | cost' == oldCost = (cost', source' : oldPreds)
              | cost' < oldCost = (cost', [source'])
              | otherwise = (oldCost, oldPreds)
            upd' = ((),) . Just . upd -- PQ.alter insists on calculating an extra value

solve :: Int -> [[Cell]] -> Maybe (Int, Int)
solve width cells = do
  let assocMaze = A.assocs maze
  start <- fst <$> L.find (\(_, c) -> c == Start) assocMaze
  end <- fst <$> L.find (\(_, c) -> c == End) assocMaze

  let initialState = MazeNode {pos = start, dir = East}
      spt = dijkstra maze initialState
  (minCost, endNodes) <- cheapestNodesByPos spt end
  let paths = dfs (preds <$> spt) endNodes
  
  return (minCost, (L.length . nubOrdOn pos) paths)
  where
    maze = A.listArray ((0, 0), (width - 1, width - 1)) (join $ L.transpose cells)

    -- Because direction is part of the node, finding the cheapest end nodes is a bit involved
    cheapestNodesByPos spt pos' = do
      let nodes = [MazeNode {pos = pos', dir = dir'} | dir' <- [minBound .. maxBound]]
      let withCosts = mapMaybe (\n -> (n,) <$> M.lookup n spt) nodes
      minimumCost <- minimumMay $ cost . snd <$> withCosts
      let nodesOnShortestPath = [n | (n, SptValue c _) <- withCosts, c == minimumCost]
      return (minimumCost, nodesOnShortestPath)

main :: IO ()
main = do
  (mazeFile : _) <- getArgs
  (width, maze) <- parseFileWith (grid cell <* eof) mazeFile
  case solve width maze of
    Nothing -> putStrLn "No solution found"
    Just (shortestPathLength, numberOfSittingPlaces) -> do
      putStrLn $ "Found a solution with cost " ++ show shortestPathLength
      putStrLn $ "Number of sitting places: " ++ show numberOfSittingPlaces
