import Parsing ( parseFileWith )
import System.Environment (getArgs)
import Parser (machineConfiguration)
import Interpreter (mkInterpreter, run)
import Data.List qualified as L (intersperse)
import Control.Monad (join)

toCsv :: [Integer] -> String
toCsv = join . L.intersperse "," . map show

main :: IO ()
main = do
    (machineConfigPath : _) <- getArgs 
    machineConfig <- parseFileWith machineConfiguration machineConfigPath
    let initialState = mkInterpreter machineConfig
        programOutput = toCsv . run $ initialState
    putStrLn $ "Program output: " ++ programOutput
