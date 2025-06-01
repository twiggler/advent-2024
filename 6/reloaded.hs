import AxisAligned (AlignedAxisMaps, Bounds, Coord2, addCoord, castRay, makeAlignedAxesMaps)
import Control.Monad (join)
import Cursor (CardinalDir (..))
import Data.Containers.ListUtils (nubOrdOn)
import Data.Functor (($>), (<&>))
import Data.List qualified as L (find, zipWith, drop, zip, nub, unfoldr)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE (last, tail, toList)
import Data.Maybe (fromJust)
import Data.Set qualified as S (empty, member, insert)
import Parsing (grid, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof)

data Cell = Empty | Obstruction | Start deriving (Show, Eq, Ord)

type Coord2Dir = (Int, Int, CardinalDir)

data Lab = Lab Coord2Dir Bounds AlignedAxisMaps
  deriving (Show)

cell :: ReadP Cell
cell = choice [char '.' $> Empty, char '#' $> Obstruction, char '^' $> Start]

-- Partial function, if the start cell is not found we crash
makeLab :: Int -> [[Cell]] -> Lab
makeLab width cells =
  let coords = [(x, y) | y <- [0 ..], x <- [0 .. width - 1]]
      cellsWithCoords = coords `zip` join cells
      obstacles = filterObstacles cellsWithCoords
      axisAlignedMaps = makeAlignedAxesMaps obstacles
      (sx, sy) = fromJust $ findStart cellsWithCoords
      (bounds', _) = last cellsWithCoords
   in Lab (sx, sy, North) bounds' axisAlignedMaps
  where
    filterObstacles = fmap fst . filter ((== Obstruction) . snd)
    findStart = fmap fst . L.find ((== Start) . snd)

extend :: Coord2Dir -> [Coord2Dir]
extend (x, y, dir) = case dir of
  North -> [(x, y', North) | y' <- [y, y - 1 ..]]
  East -> [(x', y, East) | x' <- [x ..]]
  South -> [(x, y', South) | y' <- [y ..]]
  West -> [(x', y, West) | x' <- [x, x - 1 ..]]

inBounds :: Bounds -> Coord2Dir -> Bool
inBounds (w, h) (x, y, _) = x >= 0 && y >= 0 && x <= w && y <= h

toCoord2 :: Coord2Dir -> Coord2
toCoord2 (x, y, _) = (x, y)

patrol :: Coord2Dir -> AlignedAxisMaps -> NonEmpty Coord2Dir
patrol start' cm =
  start' :| L.unfoldr step start'
  where
    step (x, y, dir) = do
      next <- bounce dir <$> castRay cm (x, y) dir
      return (next, next)
    bounce dir (x, y) = case dir of
      North -> (x, y + 1, East)
      East -> (x - 1, y, South)
      South -> (x, y - 1, West)
      West -> (x + 1, y, North)

solve1 :: Lab -> [Coord2Dir]
solve1 (Lab start' bounds' aam) =
  let jumps = patrol start' aam
      moves = concat $ L.zipWith lerp (NE.toList jumps) (NE.tail jumps)
      toEdge = runToEdge (NE.last jumps)
   in moves ++ toEdge
  where
    lerp from to = takeWhile (\p -> toCoord2 p /= toCoord2 to) $ extend from
    runToEdge = takeWhile (inBounds bounds') . extend

isCycle :: [Coord2Dir] -> Bool
isCycle = go S.empty
  where
    go _ [] = False
    go visited (x : xs) = S.member x visited || go (S.insert x visited) xs

solve2 :: Lab -> [Coord2Dir] -> Int
solve2 (Lab _ _ axesMaps) path =
  let candidatePositions = L.drop 1 path <&> toCoord2
      scenarios = nubOrdOn snd (L.zip path candidatePositions)
   in length $ filter (isCycle . NE.toList) (uncurry go <$> scenarios)
  where
    go start' (ox, oy) =
      let axesMaps' = addCoord (ox, oy) axesMaps
       in patrol start' axesMaps'

uniquePositions :: [Coord2Dir] -> Int
uniquePositions = length . L.nub . fmap (\(x, y, _) -> (x, y))

main :: IO ()
main = do
  (labPlanFilePath : _) <- getArgs
  (width, labPlan) <- parseFileWith (grid cell <* eof) labPlanFilePath
  let lab = makeLab width labPlan
      initialPatrol = solve1 lab
      distinctPositions = uniquePositions initialPatrol
      obstaclesWithLoop = solve2 lab initialPatrol
  putStrLn $ "Number of distinct positions visited equals " ++ show distinctPositions
  putStrLn $ "Number of obstacles trapping the guard equals  " ++ show obstaclesWithLoop
  return ()
