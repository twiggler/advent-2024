import Data.Foldable (traverse_)
import Linear (V2 (..), V4 (V4), liftI2, sumV, zero, (!!*), (!+!))
import Parsing (eol, parseFileWith, signedNumber)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, eof, sepBy, string)
import System.IO (hSetBuffering, stdin, BufferMode (..), hSetEcho)

data Quadrant = TopLeft | TopRight | BottomLeft | BottomRight
  deriving (Show, Eq)

robotConfigurations :: ReadP ([V2 Int], [V2 Int])
robotConfigurations = unzip <$> sepBy robotConfiguration eol <* eol <* eof
  where
    robotConfiguration = (,) <$> valueByKey "p" <*> (char ' ' *> valueByKey "v")
    valueByKey key = string (key ++ "=") *> coordinates
    coordinates = V2 <$> signedNumber <*> (char ',' *> signedNumber)

simulateRobots :: [V2 Int] -> [V2 Int] -> V2 Int -> Int -> [V2 Int]
simulateRobots positions velocities dims time =
    let positions' = positions !+! velocities !!* time
    in  (\p' -> liftI2 mod p' dims) <$> positions'  -- Wrap around 

safetyFactor :: [V2 Int] -> V2 Int -> Int
safetyFactor positions dims =
   let quadrants = quadrant ((`div` 2) <$> dims) <$> positions
   in product $ sumV quadrants
  where
    quadrant (V2 halfWidth halfHeight) (V2 x y)
      | x == halfWidth || y == halfHeight = zero
      | x < halfWidth = if y < halfHeight then V4 1 0 0 0 else V4 0 1 0 0
      | otherwise = if y < halfHeight then V4 0 0 1 0 else V4 0 0 0 1

-- An interactive simulation, disgusting.
-- It allows exploring how to identify the picture of a Christmas tree.
-- I found the time that shows the Christmas tree using my eyes.
-- Although this can be automated, the solution feels ad-hoc so I left it as is.     
interactive :: [V2 Int] -> [V2 Int] -> V2 Int -> Int -> IO ()
interactive positions velocities dims@(V2 width height) time = do
  let positions' = simulateRobots positions velocities dims time
  draw positions'
  cmd <- getChar
  case cmd of
    'n' -> interactive positions velocities dims (time - 1)
    'm' -> interactive positions velocities dims (time + 1)
    'j' -> interactive positions velocities dims (time - width)
    'k' -> interactive positions velocities dims (time + width)
    'r' -> interactive positions velocities dims 0
    'q' -> return () 
    _ -> interactive positions velocities dims time 
  where
    draw position = do
      clearScreen
      traverse_ drawRobot position
      setCursorPosition (height + 2) 0 
      putStrLn $ "Current time equals " ++ show time
      putStrLn "Press 'n' to decrease time by one, 'm' to decrease time by 1, q to quit"
      putStrLn "Press 'j' to decrease time by width, 'k' to increase time by width, 'r' to reset time to 0"

    drawRobot (V2 x y) = do
      setCursorPosition y x
      putChar '*'

solve :: [V2 Int] -> [V2 Int] -> V2 Int -> Int -> Int
solve positions velocities dims time =
    let positions' = simulateRobots positions velocities dims time
    in safetyFactor positions' dims    

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  (mode : robotConfigurationFile : width : height : _) <- getArgs
  (positions, velocities) <- parseFileWith robotConfigurations robotConfigurationFile
  let dims = V2 (read width) (read height)
  
  case mode of
    "solve" -> do
        let sf = solve positions velocities dims 100
        putStrLn $ "Safety factor equals: " ++ show sf
    "interactive" -> interactive positions velocities dims 0
    _ -> error "Unknown mode. Use 'solve' or 'interactive'"
 