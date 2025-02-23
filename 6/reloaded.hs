import Data.Bifunctor qualified as BF
import Data.Containers.ListUtils (nubOrdOn)
import Data.Functor (($>), (<&>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Tuple (swap)
import Parsing
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

data Cell = Empty | Obstruction | Start deriving (Show, Eq, Ord)

data CardinalDir = North | East | South | West deriving (Eq, Show, Ord)

type Coord2 = (Int, Int)

type Bounds = Coord2

type Coord2Dir = (Int, Int, CardinalDir)

type AlignedAxisMaps = (IntMap IntSet, IntMap IntSet)

data Lab = Lab
  { __ :: Coord2Dir,
    ___ :: Bounds,
    alignedAxesMaps :: AlignedAxisMaps
  }

readLabPlan :: ReadP [[Cell]]
readLabPlan = do
  firstRow <- manyTill cell' eol
  let width = length firstRow
  rest <- endBy (count width cell') eol <* eof
  return (firstRow : rest)
  where
    cell' = choice [char '.' $> Empty, char '#' $> Obstruction, char '^' $> Start]

makeLab :: [[Cell]] -> Lab
makeLab cells =
  let cellsWithCoords = withCoords cells
      obstacles = filterObstacles cellsWithCoords
      axisAlignedMaps = makeAlignedAxesMaps obstacles
      (sx, sy) = fromJust $ findStart cellsWithCoords
      (bounds', _) = last cellsWithCoords
   in Lab (sx, sy, North) bounds' axisAlignedMaps
  where
    filterObstacles = fmap fst . filter ((== Obstruction) . snd)
    findStart = fmap fst . L.find ((== Start) . snd)

withCoords :: [[Cell]] -> [(Coord2, Cell)]
withCoords = concat . zipWith withCoordY [0 ..]
  where
    withCoordY y = zipWith (withCell y) [0 ..]
    withCell y x c = ((x, y), c)

makeAlignedAxesMaps :: [Coord2] -> AlignedAxisMaps
makeAlignedAxesMaps cells =
  let xByY = IM.fromListWith IS.union (keyValue swap cells)
      yByX = IM.fromListWith IS.union (keyValue id cells)
   in (xByY, yByX)
  where
    keyValue f = fmap (BF.second IS.singleton . f)

castRay :: AlignedAxisMaps -> Coord2Dir -> Maybe Coord2Dir
castRay (xByY, yByX) (x, y, dir) = case dir of
  North -> IM.lookup x yByX >>= IS.lookupLT y >>= \y' -> Just (x, y' + 1, East)
  East -> IM.lookup y xByY >>= IS.lookupGT x >>= \x' -> Just (x' - 1, y, South)
  South -> IM.lookup x yByX >>= IS.lookupGT y >>= \y' -> Just (x, y' - 1, West)
  West -> IM.lookup y xByY >>= IS.lookupLT x >>= \x' -> Just (x' + 1, y, North)

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
    step pos = do
      next <- castRay cm pos
      return (next, next)

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
solve2 lab path =
  let candidatePositions = L.drop 1 path <&> toCoord2
      scenarios = nubOrdOn snd (L.zip path candidatePositions)
   in length $ filter (isCycle . NE.toList) (uncurry go <$> scenarios)
  where
    go start' (ox, oy) =
      let patchedCm = addObstacle (alignedAxesMaps lab) (ox, oy)
       in patrol start' patchedCm

addObstacle :: AlignedAxisMaps -> Coord2 -> AlignedAxisMaps
addObstacle (xByY, yByX) (x, y) =
  let xByY' = IM.adjust (IS.insert x) y xByY
      yByX' = IM.adjust (IS.insert y) x yByX
   in (xByY', yByX')

uniquePositions :: [Coord2Dir] -> Int
uniquePositions = length . L.nub . fmap (\(x, y, _) -> (x, y))

main :: IO ()
main = do
  (labPlanFilePath : _) <- getArgs
  labPlan <- parseFileWith readLabPlan labPlanFilePath
  let lab = makeLab labPlan
      initialPatrol = solve1 lab
      distinctPositions = uniquePositions initialPatrol
      obstaclesWithLoop = solve2 lab initialPatrol
  putStrLn $ "Number of distinct positions visited equals " ++ show distinctPositions
  putStrLn $ "Number of obstacles trapping the guard equals  " ++ show obstaclesWithLoop
  return ()
