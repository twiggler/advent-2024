import Data.List

readLocationFile :: String -> IO ([Int], [Int])
readLocationFile filename = do 
    rows <- map words . lines <$> readFile filename
    let parsedRows = [(read loc1, read loc2) | [loc1, loc2] <- rows] 
    return (unzip parsedRows)

totalDistance :: [Int] -> [Int] -> Int
totalDistance xs ys = sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)

main = do
    (xs, ys) <- readLocationFile "input"
    let d = totalDistance xs ys
    putStrLn $ "Total distance equals " ++ show d
