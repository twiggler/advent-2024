import Control.Monad (join)
import Data.Char (isAlphaNum)
import Linear.V2
import System.Environment (getArgs)

data Antenna = Antenna
  { pos :: V2 Int,
    frequency :: Char
  }
  deriving (Show)

data City = AntennaMap
  { antennas :: [Antenna],
    dim :: Int  -- Assume the city is a square
  }
  deriving (Show)

readCityMap :: String -> IO [String]
readCityMap = fmap lines . readFile

makeCity :: [String] -> City
makeCity cityMap =
  let dim' = length cityMap
      coords = [V2 x y | x <- [0 .. dim' - 1], y <- [0 .. dim' - 1]]
      antennas' = filter (isAlphaNum . frequency) $ zipWith Antenna coords (join cityMap)
   in AntennaMap antennas' dim'

main :: IO ()
main = do
  (mapFile : _) <- getArgs
  antennaMap <- readCityMap mapFile
  return ()
