import Control.Comonad (extend)
import Control.Monad (join)
import Cursor (neighbors, toMatrix, toPaddedGrid)
import Data.Graph.Inductive.Graph (mkGraph, outdeg)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (components)
import Parsing (readLines)
import System.Environment.Blank (getArgs)

data Plot = Plot
  { plantType :: Char,
    label :: Int
  }
  deriving (Show)

mkPlotGraph :: [String] -> Gr () ()
mkPlotGraph plantTypes =
  let dim = length plantTypes -- Assuming square grid
      plotLabels = [[y * dim + x | x <- [0 .. dim - 1]] | y <- [0 .. dim - 1]]
      plots = zipWith (zipWith Plot) plantTypes plotLabels

      adjacentPlots = toMatrix $ extend neighbors $ toPaddedGrid plots
      plotEdges = edges $ zip (join plots) (join adjacentPlots)
      similarPlotEdges = filter (\(Plot t1 _, Plot t2 _) -> t1 == t2) plotEdges

      lNodes = (\plot -> (label plot, ())) <$> join plots
      lEdges = (\(from, to) -> (label from, label to, ())) <$> similarPlotEdges
   in mkGraph lNodes lEdges
  where
    edges = concatMap (\(from, tos) -> [(from, to) | to <- tos])

fencingCost :: Gr () () -> Int
fencingCost gr = sum $ fmap regionCost (components gr)
  where
    regionCost r = length r * sum (fmap (\node -> 4 - outdeg gr node) r)

main :: IO ()
main = do
  (plantTypeMapFile : _) <- getArgs
  plantTypeMap <- readLines plantTypeMapFile
  let plotGraph = mkPlotGraph plantTypeMap
  let cost = fencingCost plotGraph
  putStrLn $ "The cost of fencing the garden is " ++ show cost
