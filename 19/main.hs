import Algorithm.Search (dfs)
import Data.Bifunctor qualified as BF
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B (length, null, pack)
import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import Data.IntMap qualified as M (empty, insert, lookup)
import Data.Maybe (mapMaybe)
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as PQ (alter, minView, singleton)
import Data.Trie (Trie)
import Data.Trie qualified as T (fromList, matches)
import Parsing (eol, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, endBy1, eof, munch1, sepBy1, string)

data Onsen = Onsen (Trie ()) [ByteString]

towels :: ReadP ([String], [String])
towels = (,) <$> patterns <* eol <*> designs <* eof
  where
    stripe = munch1 (`elem` "wubrg")
    patterns = sepBy1 stripe (string ", ") <* eol
    designs = endBy1 stripe eol

mkOnsen :: [String] -> [String] -> Onsen
mkOnsen patterns designs =
  let patternTrie = T.fromList [(B.pack p, ()) | p <- patterns]
      designs' = B.pack <$> designs
   in Onsen patternTrie designs'

neighbors :: Trie () -> ByteString -> [ByteString]
neighbors patternTrie state = [remainder | (_, _, remainder) <- T.matches patternTrie state]

upsert :: (Ord k, Ord p) => (p, v) -> ((p, v) -> (p, v)) -> k -> OrdPSQ k p v -> OrdPSQ k p v
upsert initial update key queue = snd $ PQ.alter f key queue
  where
    f = ((),) . Just . maybe initial update

-- Like Dijkstra's, but accumulating the number of paths to each node.
-- This works because there are only edges to shorter strings.
nPaths :: (ByteString -> [ByteString]) -> ByteString -> IntMap Integer
nPaths next start = go (PQ.singleton start (toPriority start) 1) M.empty
  where
    go queue pathCounts =
      case PQ.minView queue of
        Nothing -> pathCounts
        Just (node, priority, nPaths', queue') ->
          let pathCounts' = M.insert (fromPriority priority) nPaths' pathCounts
              queue'' = foldl' (updatedQueue nPaths') queue' (next node)
           in go queue'' pathCounts'

    toPriority = negate . B.length
    fromPriority = negate

    updatedQueue nPaths' queue dest =
      upsert (toPriority dest, nPaths') (BF.second (nPaths' +)) dest queue

solve1 :: Onsen -> Int
solve1 (Onsen patternTrie designs) =
  length $ mapMaybe (dfs (neighbors patternTrie) B.null) designs

solve2 :: Onsen -> Integer
solve2 (Onsen patternTrie designs) =
  sum $ mapMaybe (M.lookup 0 . nPaths (neighbors patternTrie)) designs

main :: IO ()
main = do
  (towelFile : _) <- getArgs
  (patterns, designs) <- parseFileWith towels towelFile
  let onsen = mkOnsen patterns designs
      possibleDesigns = solve1 onsen
      waysOfMaking = solve2 onsen
  putStrLn $ "Number of possible designs: " ++ show possibleDesigns
  putStrLn $ "Total number of ways of making designs: " ++ show waysOfMaking
