import Algorithm.Search (bfs, pruning)
import Data.Array.IArray qualified as A 
import Data.Array.Unboxed (UArray, (!?))
import Data.DisjointSet qualified as D 
import Data.List qualified as L 
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as S 
import Maze (Coord2, neighbors1)
import Parsing (eol, number, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, endBy, eof)

bytePositions :: ReadP [Coord2]
bytePositions = endBy bytePosition eol <* eof
  where
    bytePosition = (,) <$> (number <* char ',') <*> number

solve1 :: Int -> [Coord2] -> Maybe [Coord2]
solve1 dim byteCoords = bfs (neighbors1 `pruning` corrupted) (== (dim, dim)) (0, 0)
  where
    memorySpace :: UArray Coord2 Bool
    memorySpace =
      let corruptedAssocs = [((y, x), True) | (x, y) <- byteCoords]
          bounds = ((0, 0), (dim, dim))
       in A.accumArray (||) False bounds corruptedAssocs

    corrupted coord = fromMaybe True (memorySpace !? coord)

-- Use union-find on the bytes falling in reverse
solve2 :: Int -> [Coord2] -> Maybe Coord2
solve2 dim byteCoords =
  let allByteCoords = S.fromList [(x, y) | x <- [0 .. dim], y <- [0 .. dim]]
      initialByteCoords = S.difference allByteCoords (S.fromList byteCoords)
      initialMemorySpace = foldr addMemoryCell D.empty initialByteCoords
      reverseCoords = L.reverse byteCoords
      disjointSets = scanl (flip addMemoryCell) initialMemorySpace reverseCoords
      timeline = reverseCoords `L.zip` L.drop 1 disjointSets
   in fst <$> L.find (D.equivalent (0, 0) (dim, dim) . snd) timeline
  where
    neighborUnion byte neighbor ds =
      if isJust (D.representative neighbor ds) then D.union byte neighbor ds else ds

    addMemoryCell coord ds =
      foldr (neighborUnion coord) (D.insert coord ds) (neighbors1 coord)

main :: IO ()
main = do
  (mazeFile : dimStr : prefixStr : _) <- getArgs
  let (dim, prefix) = (read dimStr :: Int, read prefixStr :: Int)

  positions <- parseFileWith bytePositions mazeFile
  case solve1 dim (L.take prefix positions) of
    Just path -> putStrLn $ "Found a path of length " ++ show (length path)
    Nothing -> putStrLn "No path found."
  case solve2 dim positions of
    Just (x, y) -> putStrLn $ "Byte at " ++ show x ++ "," ++ show y ++ " prevents exit"
    Nothing -> putStrLn "No byte found that prevents exit."
