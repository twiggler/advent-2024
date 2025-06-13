import AxisAligned (AlignedAxisMaps, Coord2, addCoord, castRay, coords, makeAlignedAxesMaps, removeCoord)
import Control.Monad (guard, join)
import Cursor (CardinalDir (..))
import Data.Array (Array, (!), (//))
import Data.Array qualified as A (assocs, listArray)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor (($>))
import Data.List qualified as L (concatMap, cycle, find, foldl', partition, takeWhile, transpose, unionBy, zip)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S (fromList, notMember)
import Parsing (eol, grid, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof)

data Cell = Empty | Box | Start | Wall deriving (Show, Eq, Ord)

data CellWide = EmptyW | BoxLeft | BoxRight | StartW | WallW deriving (Show, Eq, Ord)

isWallW, isEmptyW :: (Coord2, CellWide) -> Bool
isWallW (_, t) = t == WallW
isEmptyW (_, t) = t == EmptyW

data Warehouse = Warehouse Coord2 AlignedAxisMaps (Set Coord2)
  deriving (Show)

-- While less performant than Vector, Array makes multi-dimensional access easy
type WarehouseGridW = Array Coord2 CellWide

-- Part 2 basically stands on its own, and requires a different representation
data WarehouseW = WideWarehouse Coord2 WarehouseGridW
  deriving (Show)

lookupCellW :: WarehouseGridW -> Coord2 -> (Coord2, CellWide)
lookupCellW grid' coord = (coord, grid' ! coord)

warehouseConfiguration :: ReadP ([[Cell]], Int, [CardinalDir])
warehouseConfiguration = do
  (width, warehouseMap) <- grid cell <* eol
  (_, movements) <- grid move <* eof
  return (warehouseMap, width, join movements)
  where
    cell = choice [char '.' $> Empty, char '#' $> Wall, char '@' $> Start, char 'O' $> Box]
    move = choice [char '<' $> West, char '>' $> East, char '^' $> North, char 'v' $> South]

-- Partial function, if the start cell is not found we crash
makeWarehouse :: Int -> [[Cell]] -> Warehouse
makeWarehouse width cells =
  let coords' = [(x, y) | y <- [0 ..], x <- [0 .. width - 1]]
      withCoords = coords' `zip` join cells
      (emptyCells, notEmpty) = L.partition (\(_, c) -> c == Empty) withCoords
      walls = [(x, y) | ((x, y), c) <- notEmpty, c == Wall]
      startPos = (fst . fromJust) $ L.find (\(_, c) -> c == Start) withCoords
      axisAlignedMaps = makeAlignedAxesMaps $ startPos : (walls ++ (fst <$> emptyCells))
   in Warehouse startPos axisAlignedMaps (S.fromList walls)

makeWideWarehouse :: Int -> [[Cell]] -> WarehouseW
makeWideWarehouse width cells =
  let bound = (width * 2 - 1, length cells - 1)
      wideCells = L.concatMap toWideCell <$> cells
      grid' = A.listArray ((0, 0), bound) (join $ L.transpose wideCells)
      startPos = (fst . fromJust) $ L.find (\(_, c) -> c == StartW) (A.assocs grid')
   in WideWarehouse startPos (grid' // [(startPos, EmptyW)])
  where
    toWideCell Empty = [EmptyW, EmptyW]
    toWideCell Box = [BoxLeft, BoxRight]
    toWideCell Start = [StartW, EmptyW]
    toWideCell Wall = [WallW, WallW]

simulate :: [CardinalDir] -> Warehouse -> Warehouse
simulate moves start = L.foldl' (\house dir -> fromMaybe house (move dir house)) start moves
  where
    move :: CardinalDir -> Warehouse -> Maybe Warehouse
    move dir (Warehouse (x, y) am walls) = do
      freeCell <- castRay am (x, y) dir
      guard $ freeCell `S.notMember` walls
      let nextPos = case dir of
            North -> (x, y - 1)
            East -> (x + 1, y)
            South -> (x, y + 1)
            West -> (x - 1, y)
          am' = (addCoord nextPos . removeCoord freeCell) am
      return $ Warehouse nextPos am' walls

-- Do a breadth-first search to find all boxes that can be moved.
-- The warehouse is guaranteed to be closed off by walls, so no boundary checks are needed.
boxes :: WarehouseGridW -> Int -> [Coord2] -> Maybe [Coord2]
boxes _ _ [] = return []
boxes grid' dy needles = do
  let cells = lookupCellW grid' <$> needles
      boxes' = [coord | (coord, cell) <- cells, cell == BoxLeft]
  guard $ notElem WallW [grid' ! c | c <- flood boxes' [0, 1]]
  children <- boxes grid' dy (nubOrd $ flood boxes' [0, 1, -1])
  return $ boxes' ++ children
  where
    flood xs offs = [(bx + dx, by + dy) | (bx, by) <- xs, dx <- offs]

simulateW :: [CardinalDir] -> WarehouseW -> WarehouseW
simulateW moves start = L.foldl' (\house dir -> fromMaybe house (move dir house)) start moves
  where
    -- Can be optimized by using Data.Array.ST at the cost of purity
    move :: CardinalDir -> WarehouseW -> Maybe WarehouseW
    move North w = moveVertical (-1) w
    move South w = moveVertical 1 w
    move West w = moveHorizontal (-1) [BoxRight, BoxLeft] w
    move East w = moveHorizontal 1 [BoxLeft, BoxRight] w

    moveVertical dy (WideWarehouse (x, y) grid') = do
      guard $ grid' ! (x, y + dy) /= WallW -- Check robot does not hit a wall
      boxes' <- boxes grid' dy [(x - 1, y + dy), (x, y + dy)]
      let boxU = L.concatMap (\(bx, by) -> boxUpdates (bx, by + dy)) boxes'
          emptyU = L.concatMap emptyUpdates boxes'
          -- Union with box positions which remain empty after the move
          updates = L.unionBy (\(p1, _) (p2, _) -> p1 == p2) boxU emptyU
      return $ WideWarehouse (x, y + dy) (grid' // updates)
      where
        boxUpdates (bx, by) = [((bx, by), BoxLeft), ((bx + 1, by), BoxRight)]
        emptyUpdates (bx, by) = [((bx, by), EmptyW), ((bx + 1, by), EmptyW)]

    moveHorizontal dx ws (WideWarehouse (x, y) grid') =
      case maybeEmptyX of
        Just emptyX -> Just $ WideWarehouse (x', y') (grid' // updates)
          where
            (x', y') = (x + dx, y)
            boxes' = [x' + dx, x' + 2 * dx .. emptyX]
            boxU = [((bx, y), w) | (bx, w) <- boxes' `L.zip` L.cycle ws]
            updates = ((x', y'), EmptyW) : boxU
        Nothing -> Nothing
      where
        row = [lookupCellW grid' (wx, y) | wx <- [x + dx, x + 2 * dx ..]]
        maybeEmptyX = fst . fst <$> L.find isEmptyW (L.takeWhile (not . isWallW) row)

boxGpsSum :: Int -> Warehouse -> Int
boxGpsSum dim (Warehouse _ am _) =
  let maxBoxScore = (101 * dim) * sum [0 .. dim - 1] -- (100 * width + width) * ...
      nonBoxScore = sum [x + 100 * y | (x, y) <- coords am]
   in maxBoxScore - nonBoxScore

boxGpsSumWide :: WarehouseW -> Int
boxGpsSumWide (WideWarehouse _ grid') =
  let boxCoords = [coord | (coord, cell) <- A.assocs grid', cell == BoxLeft]
   in sum [y * 100 + x | (x, y) <- boxCoords]

main :: IO ()
main = do
  (warehousePlanFile : _) <- getArgs
  (cells, dim, moves) <- parseFileWith warehouseConfiguration warehousePlanFile
  let warehouse = makeWarehouse dim cells
      finalWarehouse = simulate moves warehouse
      gpsSum = boxGpsSum dim finalWarehouse
  putStrLn $ "The sum of the GPS coordinates of the boxes is: " ++ show gpsSum

  let wideWarehouse = makeWideWarehouse dim cells
      finalWideWarehouse = simulateW moves wideWarehouse
      gpsSumWide = boxGpsSumWide finalWideWarehouse
  putStrLn $ "The sum of the GPS coordinates of the wide boxes is: " ++ show gpsSumWide
