import AxisAligned (AlignedAxisMaps, Coord2, addCoord, castRay, makeAlignedAxesMaps, removeCoord, coords)
import Control.Monad (guard, join)
import Cursor (CardinalDir (..))
import Data.Functor (($>))
import Data.List qualified as L (filter, find, foldl', partition)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S (fromList, notMember)
import Parsing (eol, grid, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof)

data Cell = Empty | Box | Start | Wall deriving (Show, Eq, Ord)

data Warehouse = Warehouse Coord2 AlignedAxisMaps (Set Coord2)
  deriving (Show)

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
      walls = fst <$> L.filter (\(_, c) -> c == Wall) notEmpty
      (startPos, _) = fromJust $ L.find (\(_, c) -> c == Start) withCoords
      axisAlignedMaps = makeAlignedAxesMaps $ startPos : (walls ++ (fst <$> emptyCells))
   in Warehouse startPos axisAlignedMaps (S.fromList walls)

simulate :: [CardinalDir] -> Warehouse -> Warehouse
simulate moves start = L.foldl' (\w d -> fromMaybe w (move d w)) start moves
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

boxGpsSum :: Int -> Warehouse -> Int
boxGpsSum dim (Warehouse _ am _) =
  let maxBoxScore = (101 * dim) * sum [0 .. dim - 1] -- (100 * width + width) * ...
      nonBoxScore = L.foldl' (\s (x, y)  -> s + x + 100 * y) 0 (coords am)
   in maxBoxScore - nonBoxScore

main :: IO ()
main = do
  (warehousePlanFile : _) <- getArgs
  (cells, dim, moves) <- parseFileWith warehouseConfiguration warehousePlanFile
  let warehouse = makeWarehouse dim cells
      finalWarehouse = simulate moves warehouse
      gpsSum = boxGpsSum dim finalWarehouse
  putStrLn $ "The sum of the GPS coordinates of the boxes is: " ++ show gpsSum
