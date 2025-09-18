import Control.Monad (foldM, guard, join)
import Data.List qualified as L
import Interpreter (mkInterpreter, run)
import Machine (MachineConfig (..), ToWords (toWords))
import Parser (machineConfiguration)
import Parsing (parseFileWith)
import Safe (minimumMay)
import System.Environment (getArgs)

solve1 :: MachineConfig -> String
solve1 = join . L.intersperse "," . fmap (show . toInteger) . run . mkInterpreter

-- Assumptions:
--  * The state of the machine at any step is a function of register A.
--  * register A is shifted right by 3 bits at each step.
solve2 :: MachineConfig -> Maybe Integer
solve2 config = do
  let instructionWords = toWords `concatMap` instructions config
      candidates = foldM findPreviousA 0 (reverse instructionWords)
  smallest <- minimumMay candidates
  guard $ run (mkInterpreter config {initialA = smallest}) == instructionWords
  return smallest
  where
    findPreviousA nextA out = filter (\a -> runWithA a == out) [nextA * 8 .. nextA * 8 + 7]
    runWithA a = (head . run . mkInterpreter) (config {initialA = a})

main :: IO ()
main = do
  (machineConfigPath : _) <- getArgs
  machineConfig <- parseFileWith machineConfiguration machineConfigPath
  let programOutput = solve1 machineConfig
      initialRegA = solve2 machineConfig
  putStrLn $ "Program output: " ++ programOutput
  putStrLn $ "Start value of registry A is: " ++ show initialRegA
