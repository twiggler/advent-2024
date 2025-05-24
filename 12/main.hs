import Control.Comonad (extend)
import Control.Monad (join)
import Cursor (Dir2, cardinalDirs, neighbors, reverseDir2, toMatrix, toPaddedGrid)
import Data.Graph.Inductive (edgeLabel)
import Data.Graph.Inductive.Graph (Node, deg, inn, mkGraph, out, pre)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (components)
import Data.List ((\\))
import Parsing (readLines)
import System.Environment.Blank (getArgs)

data Plot = Plot
  { plantType :: Char,
    label :: Int
  }
  deriving (Show)

type PlotGraph = Gr () Dir2

type PricingModel = PlotGraph -> [Node] -> Int

mkPlotGraph :: [String] -> PlotGraph
mkPlotGraph plantTypes =
  let dim = length plantTypes -- Assuming square grid
      plotLabels = [[y * dim + x | x <- [0 .. dim - 1]] | y <- [0 .. dim - 1]]
      plots = zipWith (zipWith Plot) plantTypes plotLabels

      adjacentPlots = toMatrix $ extend neighbors $ toPaddedGrid plots
      plotEdges = edges $ zip (join plots) (join adjacentPlots)
      similarPlotEdges = filter isConnected plotEdges

      lNodes = (\plot -> (label plot, ())) <$> join plots
      lEdges = (\(from, to, dir) -> (label from, label to, dir)) <$> similarPlotEdges
   in mkGraph lNodes lEdges
  where
    edges = concatMap (\(from, tos) -> [(from, to, dir) | (dir, to) <- tos])
    -- id inequality makes the graph directed
    isConnected (Plot t1 id1, Plot t2 id2, _) = t1 == t2 && id1 < id2

fencingCosts :: PricingModel -> PlotGraph -> Int
fencingCosts pricingModel gr = sum $ regionCost <$> components gr
  where
    regionCost r = length r * pricingModel gr r

pricingModel1 :: PricingModel
pricingModel1 gr r = sum (fmap (\node -> 4 - deg gr node) r)

pricingModel2 :: PricingModel
pricingModel2 gr r = sum (sides <$> r)
  where
    sides p = length $ fences p \\ concatMap fences (pre gr p)
    fences p =
      let outDirs = edgeLabel <$> out gr p
          inDirs = reverseDir2 . edgeLabel <$> inn gr p
       in cardinalDirs \\ (outDirs ++ inDirs)

main :: IO ()
main = do
  (plantTypeMapFile : _) <- getArgs
  plantTypeMap <- readLines plantTypeMapFile
  let plotGraph = mkPlotGraph plantTypeMap
  let cost1 = fencingCosts pricingModel1 plotGraph
  let cost2 = fencingCosts pricingModel2 plotGraph
  putStrLn $ "The cost of fencing the garden using model 1 is " ++ show cost1
  putStrLn $ "The cost of fencing the garden using model 2 is " ++ show cost2
