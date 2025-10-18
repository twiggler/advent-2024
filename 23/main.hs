import Data.Char (isAsciiLower)
import Data.Containers.ListUtils (nubOrd)
import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.NodeMap (mkMapGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.IntSet (IntSet)
import Data.IntSet qualified as S
import Data.List qualified as L
import Data.Ord (comparing)
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

maximalCliques :: ComputerNetwork -> [IntSet]
maximalCliques network = go S.empty (S.fromList (G.nodes network)) S.empty
  where
    go clique candidates excluded
      | S.null candidates && S.null excluded = [clique]
      | otherwise =
          (concat . snd) $ L.mapAccumL expand (candidates, excluded) (S.toList candidates)
      where
        expand (candidates', excluded') node =
          let neighbors = S.fromList (G.suc network node)
              newClique = S.insert node clique
              newCandidates = candidates' `S.intersection` neighbors
              newExcluded = excluded' `S.intersection` neighbors
              newState = (S.delete node candidates', S.insert node excluded')
           in (newState, go newClique newCandidates newExcluded)

solve1 :: ComputerNetwork -> Int
solve1 network =
  let suspectNodes = fst <$> filter (L.isPrefixOf "t" . snd) (G.labNodes network)
   in length $ findTriangles network suspectNodes

solve2 :: ComputerNetwork -> Maybe String
solve2 network =
  let maximalCliques' = maximalCliques network
      maximumClique = L.maximumBy (comparing S.size) maximalCliques'
      cliqueNames = traverse (G.lab network) (S.toList maximumClique)
   in L.intercalate "," . L.sort <$> cliqueNames

main :: IO ()
main = do
  (networkFilePath : _) <- getArgs
  networkMap <- parseFileWith readNetworkMap networkFilePath
  let computerNetwork = mkComputerNetwork networkMap
      
  let numberOfTriangles = solve1 computerNetwork
  putStrLn $ "Number of triangles involving at least one 't' computer: " ++ show numberOfTriangles
  
  case solve2 computerNetwork of
    Nothing -> putStrLn "No maximal clique found."
    Just password -> putStrLn $ "Password equals: " ++ password
