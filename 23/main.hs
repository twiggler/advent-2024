import Data.Char (isAsciiLower)
import Data.Containers.ListUtils (nubOrd)
import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.NodeMap (mkMapGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.IntSet (IntSet, (\\))
import Data.IntSet qualified as S
import Data.List qualified as L
import Data.Ord (Down (Down), comparing)
import Data.Tuple (swap)
import Parsing (eol, parseFileWith)
import Safe (maximumByMay)
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
      -- fgl models undirected graphs by having edges in both directions
      edges = connections ++ (swap <$> connections)
      ledges = uncurry (,,()) <$> edges
   in fst $ mkMapGraph nodes ledges

-- Algorithm taken from Chiba & Nishizeki (1985)
findTriangles :: ComputerNetwork -> [Int] -> [Triangle]
findTriangles network nodes =
  let nodes' = L.sortOn (Down . G.outdeg network) nodes
   in (concat . snd) $ L.mapAccumL findForNode network nodes'
  where
    findForNode network' node =
      let (ctx, rest) = G.match node network'
       in (rest, findAmongNeighbors rest ctx)

    findAmongNeighbors _ Nothing = []
    findAmongNeighbors network' (Just ctx) =
      let neighbors = G.suc' ctx
          -- Instead of checking all pairs of this neighborhood, check the
          -- nodes in the neighborhoods of these neighbors, which is more efficient
          findCommon' = findCommon (G.node' ctx) network'
       in (concat . snd) $ L.mapAccumL findCommon' (S.fromList neighbors) neighbors

    findCommon first network' marked second =
      let neighbors = S.fromList (G.suc network' second)
          thirdVertices = S.intersection neighbors marked
       in (S.delete second marked, (first,second,) <$> S.toList thirdVertices)

-- Bron–Kerbosch algorithm with pivoting
maximalCliques :: ComputerNetwork -> [IntSet]
maximalCliques network = go S.empty (S.fromList (G.nodes network)) S.empty
  where
    go clique candidates excluded
      | S.null candidates && S.null excluded = [clique]
      | otherwise =
          -- Reduce the search space by deferring the exploration of neighbors of the pivot
          -- until after exploring the pivot itself, and the non-neighbors of the pivot
          let pivotCandidates = candidates `S.union` excluded
              pivot = L.maximumBy (comparing (G.outdeg network)) (S.toList pivotCandidates)
              iterationSet = candidates \\ S.fromList (G.suc network pivot)
           in (concat . snd) $ L.mapAccumL expand (candidates, excluded) (S.toList iterationSet)
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
solve2 network = do
  let maximalCliques' = maximalCliques network
  maximumClique <- maximumByMay (comparing S.size) maximalCliques'
  cliqueNames <- traverse (G.lab network) (S.toList maximumClique)
  return $ (L.intercalate "," . L.sort) cliqueNames

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
