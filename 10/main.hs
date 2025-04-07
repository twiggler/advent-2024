import Control.Comonad (extend)
import Control.Monad (foldM)
import Cursor
import Data.Char (digitToInt)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

type ID = (Int, Int)

data Cell = Cell {height :: Int, id' :: ID}
  deriving (Show)

readHeightData :: String -> IO [[Int]]
readHeightData file = do
  contents <- readFile file
  return $ (fmap . fmap) digitToInt (lines contents)

mkTopoMap :: [[Int]] -> PaddedGrid Cell
mkTopoMap = toPaddedGrid . zipWith (\y -> zipWith (tagCell y) [0 ..]) [0 ..]
  where
    tagCell y x cell' = Cell cell' (x, y)

branch :: FocussedGrid Cell -> Int -> [FocussedGrid Cell]
branch grid' h =
  let branches' = (flip moveGrid . fromFocussedGrid) grid' <$> cardinalDirs
   in filter ((== h) . height . focus) $ mapMaybe toFocussedGrid branches'

trailHeadScore :: PaddedGrid Cell -> Maybe [ID]
trailHeadScore grid = case toFocussedGrid grid of
  Just fg ->
    if (height . focus) fg == 0
      then Just $ id' . focus <$> foldM branch fg [1 .. 9]
      else Just []
  Nothing -> Nothing

trailHeadScores :: ([ID] -> Int) -> PaddedGrid Cell -> Int
trailHeadScores rate = sum . fmap (sum . fmap rate) . toMatrix . extend trailHeadScore

main :: IO ()
main = do
  (mapFile : _) <- getArgs
  heightData <- readHeightData mapFile
  let topoMap = mkTopoMap heightData
  let sum1 = trailHeadScores (length . nubOrd) topoMap
  let sum2 = trailHeadScores length topoMap
  putStrLn $ "Sum of the rating of trailheads using scoring method 1 equals " ++ show sum1
  putStrLn $ "Sum of the rating of trailheads using scoring method 2 equals " ++ show sum2
