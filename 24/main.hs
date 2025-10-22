import Control.Monad (join)
import Data.Char (isAsciiLower, isDigit)
import Data.Functor (($>))
import Data.List qualified as L
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Parsing (eol, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, (+++))
import Text.ParserCombinators.ReadP qualified as P
import Data.Bits (shiftL)
import Text.Read (readMaybe)

type Wire = String

data Operation = AND | OR | XOR
  deriving (Show, Eq)

data LogicGate = LogicGate
  { operation :: Operation,
    input1 :: Wire,
    input2 :: Wire,
    output :: Wire
  }
  deriving (Show, Eq)

type InitialState = ([(Wire, Bool)], [LogicGate])

type FruitMonitor = Map Wire (Maybe Bool)

readFruitMonitor :: ReadP InitialState
readFruitMonitor = do
  initialWires <- P.endBy readWireState eol <* eol
  gates <- P.endBy readGates eol <* P.eof
  return (initialWires, gates)
  where
    readWireState = (,) <$> readWire <* P.string ": " <*> readBool

    readWire = P.count 3 (P.satisfy (\c -> isDigit c || isAsciiLower c))

    readBool = (P.char '1' >> return True) +++ (P.char '0' >> return False)

    readGates = do
      source1 <- readWire <* P.char ' '
      op <- readOp <* P.char ' '
      source2 <- readWire <* P.char ' '
      dest <- P.string "-> " *> readWire
      return $ LogicGate op source1 source2 dest

    readOp = (P.string "AND" $> AND) +++ (P.string "OR" $> OR) +++ (P.string "XOR" $> XOR)

simulateLogicGate :: FruitMonitor -> LogicGate -> Maybe Bool
simulateLogicGate monitor (LogicGate op inputWire1 inputWire2 _) = do
  inputValue1 <- join $ monitor !? inputWire1
  inputValue2 <- join $ monitor !? inputWire2
  return $ case op of
    AND -> inputValue1 && inputValue2
    OR -> inputValue1 || inputValue2
    XOR -> inputValue1 /= inputValue2

mkFruitMonitor :: InitialState -> FruitMonitor
mkFruitMonitor (initialWires, gates) =
  -- detect cycles?
  let initial = M.fromList [(wire, Just value) | (wire, value) <- initialWires]
      derived =
        M.fromList
          [ (key, simulateLogicGate monitor gate)
            | gate <- gates,
              let key = output gate
          ]
      monitor = initial `M.union` derived
   in monitor

solve1 :: InitialState -> Maybe Int
solve1 initialState =
  let monitor = mkFruitMonitor initialState
      zWires = M.filterWithKey (\k _ -> "z" `L.isPrefixOf` k) monitor
   in fmap sum (M.traverseWithKey shift zWires)
    where
        shift :: Wire -> Maybe Bool -> Maybe Int
        shift wire val = do
            -- Perhaps use Integer?
            index <- ("z" `L.stripPrefix` wire) >>= readMaybe
            (`shiftL` index) . fromEnum <$> val

main :: IO ()
main = do
  (fruitMonitorFile : _) <- getArgs
  fruitMonitorState <- parseFileWith readFruitMonitor fruitMonitorFile

  let monitorOutput = solve1 fruitMonitorState
  case monitorOutput  of
    Just value -> putStrLn $ "Monitor output: " ++ show value
    Nothing -> putStrLn "Could not compute monitor output."
