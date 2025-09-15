import Data.HashMap.Lazy ((!))
import Maze
  ( Coord2,
    Maze (..),
    isWall,
    loadMaze,
    neighbors,
    neighbors1,
  )
import Search (bfs, path, prune, pruneAssoc, steps)
import System.Environment (getArgs)

-- In retrospect, this solution is more complex than required.
-- The puzzle states "there is only a single path from the start to the end",
-- which should be interpreted as there *being no dead ends*. So the puzzle input is never a maze.
-- Cheats therefore basically always end at this single path.
-- Oh well, the solution is still quite efficient.
solve :: Int -> Int -> Maze -> Int
solve efficiencyThreshold cheatRadius (Maze cells' start' end') =
  let spt = bfs (neighbors1 `prune` isWall cells') end'
      endToStart = path spt start'
      stepsToEnd = steps spt
      cheatStart = drop efficiencyThreshold (endToStart `zip` [0 ..])
      savings = savingsAtPos stepsToEnd `concatMap` cheatStart
   in length $ filter (>= efficiencyThreshold) savings
  where
    cheat' = cheat cheatRadius `pruneAssoc` (isWall cells' . fst)

    savingsAtPos stepsToEnd (pos, cost) =
      [ cost - ((stepsToEnd ! cheatPos) + cheatCost)
        | (cheatPos, cheatCost) <- cheat' pos
      ]

cheat :: Int -> Coord2 -> [(Coord2, Int)]
cheat maxTime =
  let diamond = [(offset, time) | time <- [2 .. maxTime], offset <- neighbors time (0, 0)]
   in \(px, py) -> [((px + ox, py + oy), time) | ((ox, oy), time) <- diamond]

report :: Int -> Int -> Int -> IO ()
report minSavings cheatRule numberOfCheats =
  putStrLn $
    "Number of cheats saving at least "
      ++ show minSavings
      ++ " picoseconds using cheat rule "
      ++ show cheatRule
      ++ " equals: "
      ++ show numberOfCheats

main :: IO ()
main = do
  [mazeFile, thresholdStr] <- getArgs
  maze <- loadMaze mazeFile
  let threshold = read thresholdStr
      numberOfCheats1 = solve threshold 2 maze
      numberOfCheats2 = solve threshold 20 maze
  report threshold 1 numberOfCheats1
  report threshold 2 numberOfCheats2
