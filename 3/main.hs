import Data.Char
import Data.Functor
import System.Environment
import Text.ParserCombinators.ReadP
import Parsing

data Calculator = Calculator
  { acc :: Int,
    skippedAcc :: Int,
    mulEnabled :: Bool
  }
  deriving (Show)

data Instruction = Mul Int Int | Enable | Disable deriving (Show)

updateAcc, updateSkippedAcc :: (Int -> Int) -> Calculator -> Calculator
updateAcc f calc = calc {acc = f $ acc calc}
updateSkippedAcc f calc = calc {skippedAcc = f $ skippedAcc calc}

evalInstruction :: Instruction -> Calculator -> Calculator
evalInstruction Enable calc = calc {mulEnabled = True}
evalInstruction Disable calc = calc {mulEnabled = False}
evalInstruction (Mul x y) calc = update (+ x * y) calc
  where
    update = if mulEnabled calc then updateAcc else updateSkippedAcc

runProgram :: [Instruction] -> Calculator
runProgram = foldl (flip evalInstruction) (Calculator 0 0 True)

upTo1 :: Int -> ReadP a -> ReadP [a]
-- Greedy combinator that matches at least one time and at most n times
upTo1 0 _ = pfail
upTo1 n p = count n p <++ upTo1 (n - 1) p

operand :: ReadP Int
operand = read <$> upTo1 3 (satisfy isDigit)

doInstruction :: ReadP Instruction
doInstruction = string "do()" $> Enable

dontInstruction :: ReadP Instruction
dontInstruction = string "don't()" $> Disable

mulInstruction :: ReadP Instruction
mulInstruction = string "mul" *> between (char '(') (char ')') arguments
  where
    arguments = Mul <$> operand <*> (char ',' *> operand)

withRecovery :: ReadP a -> ReadP a
-- Combinator that on failure, advances a character and tries again (and again and again ...)
withRecovery p = p <++ (satisfy (const True) *> withRecovery p)

memoryDump :: ReadP [Instruction]
memoryDump = many $ withRecovery instruction
  where
    instruction = mulInstruction +++ doInstruction +++ dontInstruction

main :: IO ()
main = do
  (memoryDumpFilePath : _) <- getArgs
  instructions <- parseFileWith memoryDump memoryDumpFilePath 
  let (Calculator enabledSum disabledSum _) = runProgram instructions
      totalSum = enabledSum + disabledSum

  putStrLn $ "Sum of enabled and disabled instruction equals " ++ show totalSum
  putStrLn $ "Sum of enabled instructions equals " ++ show enabledSum
