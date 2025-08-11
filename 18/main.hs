import Algorithm.Search (bfs, pruning)
import AxisAligned (Coord2)
import Data.Array.IArray qualified as A (accumArray)
import Data.Array.Unboxed (UArray, (!?))
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Parsing (eol, number, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, endBy, eof)

bytePositions :: ReadP [Coord2]
bytePositions = endBy bytePosition eol <* eof
  where
    bytePosition = (,) <$> (number <* char ',') <*> number

solve1 :: Int -> [Coord2] -> Maybe [Coord2]
solve1 dim byteCoords = bfs (next `pruning` corrupted) (== (dim, dim)) (0, 0)
  where
    memorySpace :: UArray Coord2 Bool
    memorySpace =
      let corruptedAssocs = [((y, x), True) | (x, y) <- byteCoords]
          bounds = ((0, 0), (dim, dim))
       in A.accumArray (||) False bounds corruptedAssocs

    next (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], abs dx + abs dy == 1]
    corrupted coord = fromMaybe True (memorySpace !? coord)

main :: IO ()
main = do
  (mazeFile : dimStr : prefixStr : _) <- getArgs
  let (dim, prefix) = (read dimStr :: Int, read prefixStr :: Int)

  positions <- parseFileWith bytePositions mazeFile
  case solve1 dim (L.take prefix positions) of
    Just path -> putStrLn $ "Found a path of length " ++ show (length path)
    Nothing -> putStrLn "No path found."
