import System.Environment  
import Data.List
import qualified Data.Map.Strict as Map

readLocationFile :: String -> IO ([Int], [Int])
readLocationFile filename = do 
    rows <- map words . lines <$> readFile filename
    let parsedRows = [(read loc1, read loc2) | [loc1, loc2] <- rows] 
    return (unzip parsedRows)

totalDistance :: [Int] -> [Int] -> Int
totalDistance xss yss = sum $ zipWith (\x y -> abs (x - y)) xss yss

similarityScore :: [Int] -> [Int] -> Int
similarityScore xss yss = sum [x * Map.findWithDefault 0 x freqMap | x <- xss]
    where freqMap = Map.fromAscListWith (+) [(y, 1) | y <- yss]

main = do
    (locationFilePath:_) <- getArgs
    (xs, ys) <- readLocationFile locationFilePath
    let 
        (xss, yss) = (sort xs, sort ys)
        dist = totalDistance xss yss
        similarity = similarityScore xss yss
    putStrLn $ "Total distance equals " ++ show dist
    putStrLn $ "Similarity score equals " ++ show similarity
