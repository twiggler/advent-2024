import Data.Char (isAsciiLower)
import Data.Containers.ListUtils (nubOrd)
import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.NodeMap (mkMapGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List qualified as L
import Data.Tuple (swap)
import Parsing (eol, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

type ComputerNetwork = Gr String ()

type Triangle = (Int, Int, Int)

readNetworkMap :: ReadP [(String, String)]
readNetworkMap = connection `P.endBy` eol
  where
    connection = (,) <$> (name <* P.char '-') <*> name
    name = P.count 2 (P.satisfy isAsciiLower)

mkComputerNetwork :: [(String, String)] -> ComputerNetwork
mkComputerNetwork connections =
  let nodes = (nubOrd . uncurry (++) . unzip) connections
      edges = connections ++ (swap <$> connections)
      ledges = uncurry (,,()) <$> edges
   in fst $ mkMapGraph nodes ledges

findTriangles :: ComputerNetwork -> [Int] -> [Triangle]
findTriangles network nodes =
  (concat . snd) $ L.mapAccumL findFromNode network nodes
  where
    findFromNode network' node = findFromCtx (G.match node network')

    findFromCtx (Nothing, network') = (network', [])
    findFromCtx (Just ctx, network') =
      let neighbors' = G.suc' ctx
          neighborPairs = [(y, z) | y : ys <- L.tails neighbors', z <- ys]
          triangles = [(G.node' ctx, y, z) | (y, z) <- neighborPairs, G.hasEdge network' (y, z)]
       in (network', triangles)

solve1 :: ComputerNetwork -> Int
solve1 network =
  let suspectNodes = fst <$> filter (L.isPrefixOf "t" . snd) (G.labNodes network)
   in length $ findTriangles network suspectNodes

main :: IO ()
main = do
  (networkFilePath : _) <- getArgs
  networkMap <- parseFileWith readNetworkMap networkFilePath
  let computerNetwork = mkComputerNetwork networkMap
      numberOfTriangles = solve1 computerNetwork

  putStrLn $ "Number of triangles involving at least one 't' computer: " ++ show numberOfTriangles
