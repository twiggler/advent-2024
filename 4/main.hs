import Data.List
import System.Environment

readCharSquare :: String -> IO [[Char]]
readCharSquare filename = lines <$> readFile filename

diagonal :: [[Char]] -> [Char]
diagonal [] = []
diagonal ([] : _) = [] -- Guard against ragged arrays
diagonal (x : xs) = head x : diagonal (tail <$> xs)

diagonals, antiDiagonals :: [[Char]] -> [[Char]]
diagonals [] = []
diagonals xs = diagonal <$> xs : subMatrix xs ++ subMatrix (transpose xs)
  where
    subMatrix = tail . init . tails
antiDiagonals = diagonals . fmap reverse

blocksOfSize :: Int -> [[Char]] -> [[[Char]]]
blocksOfSize n xs = down xs >>= (fmap transpose . down . transpose)
  where
    down ys = take n <$> (init . tails) ys

countOccurences, countWithReverse :: String -> String -> Int
countOccurences needle haystack = length $ filter (isPrefixOf needle) $ tails haystack
countWithReverse needle haystack = countOccurences needle haystack + countOccurences (reverse needle) haystack

countInSubMatrices :: String -> [[Char]] -> Int
countInSubMatrices needle m = sum $ countWithReverse needle <$> paths
  where
    paths = m ++ transpose m ++ diagonals m ++ antiDiagonals m

isXBlock :: String -> [[Char]] -> Bool
isXBlock needle block = hasNeedle (diagonal block) && hasNeedle (antiDiagonal block)
  where
    antiDiagonal = diagonal . fmap reverse
    hasNeedle x = x == needle || reverse x == needle

countInBlocks :: String -> [[Char]] -> Int
countInBlocks needle square = length $ filter (isXBlock needle) $ blocksOfSize (length needle) square

main :: IO ()
main = do
  (charSquareFilePath : _) <- getArgs
  charSquare <- readCharSquare charSquareFilePath
  let xmasCount = countInSubMatrices "XMAS" charSquare
      xOfMasCount = countInBlocks "MAS" charSquare
  putStrLn $ "Number of occurrences of 'XMAS' in the square equals " ++ show xmasCount
  putStrLn $ "Number of occurences of a X of 'MAS' occurs equals " ++ show xOfMasCount
