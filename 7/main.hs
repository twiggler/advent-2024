import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import Math.NumberTheory.Logarithms (integerLog10)
import Parsing (eol, number, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, eof, sepBy, sepBy1, string)
import Prelude hiding (concat)

data Equation = Equation
  { lhs :: Int,
    rhs :: [Int]
  }
  deriving (Show)

type Operator = Int -> Int -> Maybe Int

readEquations :: ReadP [Equation]
readEquations = sepBy equation eol <* eol <* eof
  where
    equation = Equation <$> number <* string ": " <*> rhs'
    rhs' = sepBy1 number (char ' ')

add :: Operator
add total operand =
  let total' = total - operand
   in if total' >= 0 then Just total' else Nothing

multiply :: Operator
multiply total operand = do
  let (total', remainder) = total `quotRem` operand
  if remainder == 0 then Just total' else Nothing

concat :: Operator
concat total operand =
  let divisor = 10 ^ (integerLog10 (toInteger operand) + 1)
      (total', remainder) = total `quotRem` divisor
   in if remainder == operand then Just total' else Nothing

solveEquation :: [Operator] -> Equation -> Bool
solveEquation operators (Equation target operands) =
  0 `elem` foldM applyOps target (reverse operands)
  where
    applyOps total operand = mapMaybe (\op -> op total operand) operators

solve1 :: [Operator] -> [Equation] -> Int
solve1 operators = sum . map lhs . filter (solveEquation operators)

main :: IO ()
main = do
  (calibrationEquationsFilePath : _) <- getArgs
  calibrationEquations <- parseFileWith readEquations calibrationEquationsFilePath
  let totalCalibrationResult1 = solve1 [add, multiply] calibrationEquations
  let totalCalibrationResult2 = solve1 [add, multiply, concat] calibrationEquations
  putStrLn $ "Total calibration result using add and multiply operators equals " ++ show totalCalibrationResult1
  putStrLn $ "Total calibration result using add, multiply, and concat operators equals " ++ show totalCalibrationResult2
