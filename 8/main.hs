import Control.Monad (join)
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)
import Data.IntMap qualified as M
import Data.List (tails)
import GHC.Base (ord)
import Linear (V2 (..), (^*))
import Parsing (readLines)
import System.Environment (getArgs)

data Antenna = Antenna
  { pos :: V2 Int,
    frequency :: Char
  }
  deriving (Show)

data City = City
  { antennas :: [Antenna],
    dim :: Int -- Assume the city is a square
  }
  deriving (Show)

type ResonanceModel = Int -> V2 Int -> V2 Int -> [V2 Int]

makeCity :: [String] -> City
makeCity cityMap =
  let dim' = length cityMap
      coords = [V2 x y | x <- [0 .. dim' - 1], y <- [0 .. dim' - 1]]
      antennas' = filter (isAlphaNum . frequency) $ zipWith Antenna coords (join cityMap)
   in City antennas' dim'

pairs :: [a] -> [(a, a)]
pairs as = [(x, y) | x : ys <- tails as, y <- ys]

inBounds :: Int -> V2 Int -> Bool
inBounds dim' (V2 x y) = x >= 0 && x < dim' && y >= 0 && y < dim'

model1 :: ResonanceModel
model1 dim' a b = filter (inBounds dim') [2 * b - a, 2 * a - b]

model2 :: ResonanceModel
model2 dim' a b =
  let d = b - a
      as = takeWhile (inBounds dim') [b + (d ^* n) | n <- [0 ..]]
      bs = takeWhile (inBounds dim') [a - (d ^* n) | n <- [0 ..]]
   in as ++ bs

antinodes :: ResonanceModel -> City -> Int
antinodes model (City antennas' dim') = length $ nubOrd $ concatMap searchFreq (groupByFreq antennas')
  where
    groupByFreq as = M.fromListWith (++) [(ord $ frequency a, [pos a]) | a <- as]
    searchFreq as = concatMap (uncurry (model dim')) (pairs as)

main :: IO ()
main = do
  (mapFile : _) <- getArgs
  antennaMap <- readLines mapFile
  let city = makeCity antennaMap
      numberOfAntinodes1 = antinodes model1 city
      numberOfAntinodes2 = antinodes model2 city
  putStrLn $ "Number of unique locations with an antinode using model 1 equals " ++ show numberOfAntinodes1
  putStrLn $ "Number of unique locations with an antinode using model 2 equals " ++ show numberOfAntinodes2
