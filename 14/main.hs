import Parsing (eol, parseFileWith, signedNumber)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, eof, sepBy, string)
import Linear (V2 (..), sumV, liftI2, (^*), zero, V4 (V4))

data Quadrant = TopLeft | TopRight | BottomLeft | BottomRight
  deriving (Show, Eq)

robotConfigurations :: ReadP [(V2 Int, V2 Int)]
robotConfigurations = sepBy robotConfiguration eol <* eol <* eof
  where
    robotConfiguration = (,) <$> valueByKey "p" <*> (char ' ' *> valueByKey "v")
    valueByKey key = string (key ++ "=") *> coordinates
    coordinates = V2 <$> signedNumber <*> (char ',' *> signedNumber)

simulateRobot :: V2 Int -> V2 Int -> Int -> V2 Int -> V2 Int
simulateRobot p v t = liftI2 mod (p + v ^* t)

safetyFactor :: [(V2 Int, V2 Int)] -> V2 Int -> Int -> Int
safetyFactor robots dims t =
  let p' = [simulateRobot p v t dims | (p, v) <- robots]
      quadrants = quadrant ((`div` 2) <$> dims) <$> p'
   in product $ sumV quadrants
  where
    quadrant (V2 halfWidth halfHeight) (V2 x y)
      | x == halfWidth || y == halfHeight = zero
      | x < halfWidth = if y < halfHeight then V4 1 0 0 0 else V4 0 1 0 0
      | otherwise = if y < halfHeight then V4 0 0 1 0 else V4 0 0 0 1

main :: IO ()
main = do
  (robotConfigurationFile : width : height : _) <- getArgs
  robots <- parseFileWith robotConfigurations robotConfigurationFile
  let dims = V2 (read width) (read height)
      sf = safetyFactor robots dims 100
  putStrLn $ "Safety factor equals: " ++ show sf
  return ()
