import Data.List
import Data.Map.Strict qualified as Map
import System.Environment

readLocationFile :: String -> IO ([Int], [Int])
readLocationFile filename = do
  rows <- map words . lines <$> readFile filename
  let parsedRows = [(read loc1, read loc2) | [loc1, loc2] <- rows]
  return (unzip parsedRows)

totalDistance :: [Int] -> [Int] -> Int
totalDistance xss yss = sum $ zipWith (\x y -> abs (x - y)) xss yss

similarityScore :: [Int] -> [Int] -> Int
similarityScore xss yss = sum [x * Map.findWithDefault 0 x freqMap | x <- xss]
  where
    freqMap = Map.fromAscListWith (+) [(y, 1) | y <- yss]

main = do
  (locationFilePath : _) <- getArgs
  (locs1, locs2) <- readLocationFile locationFilePath
  let (sorted_locs1, sorted_locs2) = (sort locs1, sort locs2)
      dist = totalDistance sorted_locs1 sorted_locs2
      similarity = similarityScore sorted_locs1 sorted_locs2
  putStrLn $ "Total distance equals " ++ show dist
  putStrLn $ "Similarity score equals " ++ show similarity
