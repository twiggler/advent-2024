import Data.List
import System.Environment

readReportsFile :: String -> IO [[Int]]
readReportsFile filename = map (map read . words) . lines <$> readFile filename

isSortedBy :: (Int -> Int -> Bool) -> [Int] -> Bool
isSortedBy _ [] = True
isSortedBy _ [_] = True
isSortedBy op xs = and $ zipWith op xs (tail xs)

allIncreasing :: [Int] -> Bool
allIncreasing = isSortedBy (<)

allDecreasing :: [Int] -> Bool
allDecreasing = isSortedBy (>)

leaveOneOut :: [Int] -> [[Int]]
leaveOneOut xs = zipWith (++) (inits xs) (tail $ tails xs)

isSafeLevelDifference :: Int -> Bool
isSafeLevelDifference x = abs x >= 1 && abs x <= 3

isSafeReport :: [Int] -> Bool
isSafeReport [] = False
isSafeReport [_] = False -- Requirements are ambiguous, assuming that a report with only one reading is not safe
isSafeReport levels = all isSafeLevelDifference diffs && (allIncreasing levels || allDecreasing levels)
  where
    diffs = zipWith (-) (tail levels) levels

isSafeReportWithProblemDamper :: [Int] -> Bool
isSafeReportWithProblemDamper levels = any isSafeReport $ leaveOneOut levels

countSafeReports :: ([Int] -> Bool) -> [[Int]] -> Int
countSafeReports reportValidator reports = length $ filter reportValidator reports

main :: IO ()
main = do
  (reportsFilePath : _) <- getArgs
  reports <- readReportsFile reportsFilePath
  let safe_reports_count = countSafeReports isSafeReport reports
      safe_with_problem_damper_count = countSafeReports isSafeReportWithProblemDamper reports
  putStrLn $ "Number of safe reports equals " ++ show safe_reports_count
  putStrLn $ "Number of safe reports with problem damper equals " ++ show safe_with_problem_damper_count
