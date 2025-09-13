import Data.HashMap.Lazy ((!))
import Maze (Maze (..), isWall, loadMaze, neighbors, neighbors1)
import Search (bfs, path, prune, steps)
import System.Environment (getArgs)

solve1 :: Int -> Maze -> Int
solve1 efficiencyThreshold (Maze cells' start' end') =
  let spt = bfs (neighbors1 `prune` isWall cells') end'
      endToStart = path spt start'
      cheatStart = drop efficiencyThreshold ([0 ..] `zip` endToStart)
      cheatEnd = cheatStart >>= \(cost, sPos) -> [(cost, ePos) | ePos <- cheat sPos]
   in length $ filter (>= efficiencyThreshold) (saving spt <$> cheatEnd)
  where
    cheat = neighbors 2 `prune` isWall cells'
    saving spt (cost, pos) = cost - ((steps spt ! pos) + 2)

main :: IO ()
main = do
  (mazeFile : thresholdStr : _) <- getArgs
  maze <- loadMaze mazeFile
  let cheatPositions = case maze of
        Left err -> error err
        Right m -> solve1 (read thresholdStr) m
  putStrLn $ "Number of cheat positions saving " ++ thresholdStr ++ " picoseconds: " ++ show cheatPositions
