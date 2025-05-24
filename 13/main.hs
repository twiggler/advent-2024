import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Parsing (eol, number, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, eof, sepBy, string)

-- A system of two linear Diophantine equations in two variables.
-- The equations are of the form:
-- a1 * x + b1 * y = c1
-- a2 * x + b2 * y = c2
data LinDioph2x2 = LinDioph2x2
  { a1, b1, c1 :: Int,
    a2, b2, c2 :: Int
  }
  deriving (Show)

machineConfigurations :: ReadP [LinDioph2x2]
machineConfigurations = sepBy clawMachine eol <* eof
  where
    clawMachine = do
      (a1', a2') <- valueByKey "Button A" (expr '+') <* eol
      (b1', b2') <- valueByKey "Button B" (expr '+') <* eol
      (c1', c2') <- valueByKey "Prize" (expr '=') <* eol
      return $ LinDioph2x2 {a1 = a1', b1 = b1', c1 = c1', a2 = a2', b2 = b2', c2 = c2'}
    valueByKey key expr' = do
      _ <- string (key ++ ": ")
      x <- expr' 'X'
      _ <- string ", "
      y <- expr' 'Y'
      return (x, y)
    expr op lhs = char lhs *> char op *> number

-- Extended Euclidean algorithm to find the GCD of two integers and the Bézout coefficients
extEucl :: Int -> Int -> (Int, Int, Int)
extEucl 0 b = (b, 0, 1)
extEucl a b =
  let (g, s, t) = extEucl (b `mod` a) a
   in (g, t - (b `div` a) * s, s)

solveLinDioph2x2 :: LinDioph2x2 -> Maybe (Int, Int)
solveLinDioph2x2 (LinDioph2x2 a1' b1' c1' a2' b2' c2') = do
  let (g, s1, t1) = extEucl a1' b1'
  guard (c1' `mod` g == 0) -- If c1 is not divisible by gcd, the system has no solution.
  let h = c1' `div` g
      (x1, y1) = (s1 * h, t1 * h) -- Particular solution to first equation

  -- Substitute the general solution (x1 + k * b1/g, y1 - k * a1/g) into the second equation
  let r = a2' * b1' - b2' * a1'
  guard (r /= 0) -- If r is zero, the system has no solutions.
  let (k, kr) = (g * (c2' - a2' * x1 - b2' * y1)) `divMod` r
  guard (kr == 0) -- If k is not an integer, the system has no solution.
  return (x1 + k * b1' `div` g, y1 - k * a1' `div` g) -- Unique solution to the system

playClawMachines :: [LinDioph2x2] -> Int -> (Int, Int) -> Int
playClawMachines ms off (ca, cb) =
  sum $ mapMaybe ((fmap tokenCost . solveLinDioph2x2) . offsetMachine) ms
  where
    offsetMachine m = m {c1 = c1 m + off, c2 = c2 m + off}
    tokenCost (a, b) = a * ca + b * cb

main :: IO ()
main = do
  (clawConfigurationsFile : _) <- getArgs
  configs <- parseFileWith machineConfigurations clawConfigurationsFile
  let tokenCost = playClawMachines configs 0 (3, 1)
  let offsetTokenCost = playClawMachines configs 10000000000000 (3, 1)
  putStrLn $ "Number of tokens required to win all possible prizes equals " ++ show tokenCost
  putStrLn $ "Number of tokens required to win all possible prizes on offset machines equals " ++ show offsetTokenCost
