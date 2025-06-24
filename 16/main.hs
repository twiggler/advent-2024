import Algorithm.Search (aStar)
import AxisAligned (Coord2)
import Control.Monad (join)
import Data.Array (Array, (!))
import Data.Array qualified as A (assocs, listArray)
import Data.Functor (($>))
import Data.List qualified as L (find, transpose)
import Data.Maybe (fromJust)
import Parsing (grid, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof)

data Cell = Empty | Wall | Start | End deriving (Show, Eq, Ord)

data MazeState = MazeState Coord2 Coord2
  deriving (Show, Eq, Ord)

cell :: ReadP Cell
cell = choice [char '.' $> Empty, char '#' $> Wall, char 'S' $> Start, char 'E' $> End]

neighbors :: Array Coord2 Cell -> Coord2 -> [(Coord2, Cell, Coord2)]
neighbors maze (cx, cy) =
  [ (p, maze ! p, (dx, dy))
    | dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      abs dx + abs dy == 1, -- Only cardinal directions
      let p = (cx + dx, cy + dy)
  ]

solve :: Int -> [[Cell]] -> Maybe (Int, [MazeState])
solve width cells =
  let start = fst $ fromJust $ L.find (\(_, c) -> c == Start) (A.assocs maze)
      end = fst $ fromJust $ L.find (\(_, c) -> c == End) (A.assocs maze)
      initialState = MazeState start (1, 0)
   in aStar next cost (remaining end) solved initialState
  where
    maze = A.listArray ((0, 0), (width - 1, width - 1)) (join $ L.transpose cells)

    next (MazeState pos _) =
      [MazeState p dir | (p, cell', dir) <- neighbors maze pos, cell' /= Wall]

    cost (MazeState _ (dx1, dy1)) (MazeState _ (dx2, dy2))
      | dx1 == dx2 && dy1 == dy2 = 1 -- Same direction
      | dx1 == -dx2 || dy1 == -dy2 = 1000 * 2 + 1 -- Reverse direction
      | otherwise = 1000 + 1 -- 90 degree turn
    remaining (ex, ey) (MazeState (px, py) _) =
      abs (px - ex) + abs (py - ey)

    solved (MazeState p _) = maze ! p == End

main :: IO ()
main = do
  (mazeFile : _) <- getArgs
  (width, maze) <- parseFileWith (grid cell <* eof) mazeFile
  case solve width maze of
    Nothing -> putStrLn "No solution found"
    Just (shortestPathLength, _) -> do
      putStrLn $ "Found a solution with cost " ++ show shortestPathLength
