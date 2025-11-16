import Control.Monad (guard, join, replicateM, (>=>))
import Control.Monad.Extra (liftMaybe)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Bits (Bits (..), shiftL)
import Data.Bool (bool)
import Data.Char (isAsciiLower, isDigit)
import Data.Functor (($>))
import Data.Graph.Inductive qualified as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List qualified as L
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (Down))
import Data.Set qualified as S
import Data.Tuple (swap)
import Parsing (eol, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, (+++))
import Text.ParserCombinators.ReadP qualified as P
import Text.Read (readMaybe)

type Wire = String

data Operation = AND | OR | XOR
  deriving (Show, Eq, Ord)

data LogicGate = LogicGate
  { operation :: Operation,
    input1 :: Wire,
    input2 :: Wire,
    output :: Wire
  }
  deriving (Show, Eq)

type InitialState = ([(Wire, Bool)], [LogicGate])

data SourceGroup = X | Y
  deriving (Show, Eq, Ord)

data FruitNodeLabel = Gate Int Operation | Source SourceGroup Int | Sink Int
  deriving (Show, Eq, Ord)

type FruitMonitorGraph = Gr FruitNodeLabel Wire

type NodeMap = Map FruitNodeLabel G.Node

type InitialInputValues = Map FruitNodeLabel Bool

type FruitMonitorM = MaybeT (Reader InitialInputValues)

type FruitMonitor = Map FruitNodeLabel (FruitMonitorM Bool)

data TestVector = TestVector
  { xVal :: Int,
    yVal :: Int,
    expectedZ :: Bool
  }
  deriving (Show, Eq)

data TestCase = TestCase
  { testOutput :: FruitNodeLabel,
    inputsX :: [FruitNodeLabel],
    inputsY :: [FruitNodeLabel],
    fixtures :: [TestVector]
  }
  deriving (Show, Eq)

type PatchCircuitM = StateT FruitMonitorGraph []

hasCycle :: (G.Graph gr) => gr a b -> Bool
hasCycle g = not (all isSingleton (G.scc g))
  where
    isSingleton :: [a] -> Bool
    isSingleton [_] = True
    isSingleton _ = False

swapOutputWires :: FruitMonitorGraph -> G.Node -> G.Node -> FruitMonitorGraph
swapOutputWires gr node1 node2 =
  -- There is only output wire per gate, but it can connect to multiple destinations.
  let (outEdges1, outEdges2) = (G.out gr node1, G.out gr node2)
   in G.insEdges [(node1, dest2, wire2) | (_, dest2, wire2) <- outEdges2]
        . G.insEdges [(node2, dest1, wire1) | (_, dest1, wire1) <- outEdges1]
        . G.delEdges [(n1, n2) | (n1, n2, _) <- outEdges1 ++ outEdges2]
        $ gr

sinks :: FruitMonitor -> FruitMonitorM Int
sinks = fmap sum . sequence . mapMaybe outputValue . M.assocs
  where
    outputValue :: (FruitNodeLabel, FruitMonitorM Bool) -> Maybe (FruitMonitorM Int)
    outputValue (Sink index, valueM) =
      Just $ fmap (bool 0 (1 `shiftL` index)) valueM
    outputValue _ = Nothing

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

mkFruitMonitorGraph :: InitialState -> Maybe (FruitMonitorGraph, InitialInputValues)
mkFruitMonitorGraph (sourceAssocValue, gates) = do
  let (sourceWires, sourceValues) = unzip sourceAssocValue
  sourceNodes <- traverse validateSource sourceWires

  let gateNodes =
        [ (Left $ output gate, Gate id' (operation gate))
          | (gate, id') <- gates `zip` [0 ..]
        ]
      sinkNodes = mapMaybe (validateSink . output) gates
      wireAssocLabel = sourceNodes ++ gateNodes ++ sinkNodes

      (wires, labels) = unzip wireAssocLabel
      lnodes = [0 ..] `zip` labels
      nodeByWire = M.fromList (wires `zip` [0 ..])

  producingEdges <- concat <$> traverse (producingEdge nodeByWire) gates
  let consumingEdges = mapMaybe (consumingEdge nodeByWire) gates
      ledges = producingEdges ++ consumingEdges

  let gr = G.mkGraph lnodes ledges
      initialInputValues = M.fromList $ (snd <$> sourceNodes) `zip` sourceValues
  return (gr, initialInputValues)
  where
    validateSource wire = do
      (group, indexStr) <- L.uncons wire
      index <- readMaybe indexStr
      label <- case group of
        'x' -> Just $ Source X index
        'y' -> Just $ Source Y index
        _ -> Nothing
      return (Left wire, label)

    validateSink wire = do
      (group, indexStr) <- L.uncons wire
      index <- readMaybe indexStr
      label <- case group of
        'z' -> Just $ Sink index
        _ -> Nothing
      return (Right wire, label)

    producingEdge nodeByWire gate = do
      let inputs = [input1 gate, input2 gate]
      inputNodes <- traverse (\wire -> nodeByWire !? Left wire) inputs
      let gateNode = nodeByWire ! Left (output gate)
      return [(inputNode, gateNode, input) | (inputNode, input) <- inputNodes `zip` inputs]

    consumingEdge nodeByWire gate = do
      let gateOutput = output gate
      sinkNode <- nodeByWire !? Right gateOutput
      let gateNode = nodeByWire ! Left gateOutput
      return (gateNode, sinkNode, gateOutput)

reorderEnds :: [a] -> [a]
reorderEnds xs =
  let n = length xs
      k = (n + 1) `div` 2
      (front, back) = splitAt k xs
      revBack = reverse back
   in interleave front revBack
  where
    interleave :: [a] -> [a] -> [a]
    interleave (x : xs') (y : ys') = x : y : interleave xs' ys'
    interleave xs' [] = xs' -- For odd-length lists, the front part will be longer
    interleave [] ys' = ys' -- For completeness, though not hit in this case

simulateLogicGate :: Operation -> FruitNodeLabel -> FruitNodeLabel -> FruitMonitor -> FruitMonitorM Bool
simulateLogicGate op input1' input2' monitor = do
  inputValue1 <- liftMaybe $ monitor !? input1'
  inputValue2 <- liftMaybe $ monitor !? input2'
  case op of
    AND -> liftA2 (&&) inputValue1 inputValue2
    OR -> liftA2 (||) inputValue1 inputValue2
    XOR -> liftA2 (/=) inputValue1 inputValue2

mkFruitMonitor :: FruitMonitorGraph -> Maybe FruitMonitor
mkFruitMonitor gr =
  guard (not (hasCycle gr) && G.isSimple gr) >> monitor
  where
    monitor = M.fromList <$> traverse (step . G.context gr) (G.nodes gr)

    nodeMap = M.fromList $ G.labNodes gr

    step :: G.Context FruitNodeLabel Wire -> Maybe (FruitNodeLabel, FruitMonitorM Bool)
    step ctx = do
      let label = G.lab' ctx
      value <- case label of
        Gate _ op -> traverse (nodeMap !?) (G.pre' ctx) >>= simulateLogicGate' op
        Source _ _ -> Just $ do
          initialValues <- ask
          liftMaybe $ initialValues !? label
        Sink _ -> traverse (nodeMap !?) (G.pre' ctx) >>= sinkFromNode
      return (label, value)

    simulateLogicGate' op [in1, in2] = Just $ liftMaybe monitor >>= simulateLogicGate op in1 in2
    simulateLogicGate' _ _ = Nothing

    sinkFromNode :: [FruitNodeLabel] -> Maybe (FruitMonitorM Bool)
    sinkFromNode [backingGate] = Just $ join $ liftMaybe monitor >>= liftMaybe . M.lookup backingGate
    sinkFromNode _ = Nothing

mkTestCases :: FruitMonitorGraph -> [TestCase]
mkTestCases gr = case indices of
  [] -> []
  (carryOver : restIndices) ->
    let carryOverTest = mkTestCase carryOver (take 2 restIndices)
        otherCases =
          [ mkTestCase out' (take 3 in')
            | (out', in') <- restIndices `zip` L.tails restIndices
          ]
     in carryOverTest : otherCases
  where
    indices = L.sortOn Down $ [index | (_, Sink index) <- G.labNodes gr]

    mkTestCase outputIndex activeIndices =
      let fixtures' =
            [ TestVector x' y' z'
              | x' <- inputs activeIndices,
                y' <- inputs activeIndices,
                let z' = (x' + y') `testBit` outputIndex
            ]
       in TestCase
            { testOutput = Sink outputIndex,
              inputsX = Source X <$> activeIndices,
              inputsY = Source Y <$> activeIndices,
              fixtures = fixtures'
            }

    inputs :: [Int] -> [Int]
    inputs indices' =
      [ sum [if b then bit i else 0 | (i, b) <- zip indices' bs]
        | bs <- replicateM (length indices') [False, True]
      ]

testCaseError :: TestCase -> NodeMap -> FruitMonitorGraph -> Maybe Int
testCaseError (TestCase testOutput' inputsX' inputsY' fixtures') nodeMap gr = do
  monitor <- mkFruitMonitor gr
  sum <$> traverse (runTestVector monitor) fixtures'
  where
    zeroInput = M.fromList $ [(lbl, False) | lbl@(Source _ _) <- M.keys nodeMap]

    runTestVector monitor' (TestVector xVal' yVal' expectedZ') = do
      let inputValues =
            M.fromList $
              [(lbl, xVal' `testBit` i) | lbl@(Source X i) <- inputsX']
                ++ [(lbl, yVal' `testBit` i) | lbl@(Source Y i) <- inputsY']
      outputValueM <- monitor' !? testOutput'
      outputValue <- runReader (runMaybeT outputValueM) (M.union inputValues zeroInput)
      return $ if outputValue /= expectedZ' then 1 else 0

patchCircuitForTestCase :: NodeMap -> TestCase -> PatchCircuitM [(Wire, Wire)]
patchCircuitForTestCase nodeMap testCase = do
  gr <- get
  if found gr
    then return []
    else do
      candidate <- lift $ L.filter found (next gr)
      put candidate
      return $ liftMaybe $ swappedWires gr candidate
  where
    next gr = fromMaybe [] $ do
      inputNodes <- traverse (nodeMap !?) (inputsX testCase ++ inputsY testCase)
      outputNode <- nodeMap !? testOutput testCase
      return $
        [ swapOutputWires gr node1 node2
          | (node1, wire1) <- G.bfsnWith G.labNode' inputNodes gr,
            Gate _ _ <- [wire1],
            (node2, wire2) <- G.bfsWith G.labNode' outputNode (G.grev gr),
            node1 /= node2,
            Gate _ _ <- [wire2]
        ]

    found = (== Just 0) . testCaseError testCase nodeMap

    -- The path returned by dfs is in terms of graphs.
    swappedWires :: FruitMonitorGraph -> FruitMonitorGraph -> Maybe (Wire, Wire)
    -- Although only two wires are swapped, they may connect to multiple destinations.
    swappedWires gr1 gr2 = case S.toList $ G.edgeLabel `S.map` diff of
      [fromWire, toWire] -> Just (fromWire, toWire)
      _ -> Nothing
      where
        diff = S.fromList (G.labEdges gr1) `S.difference` S.fromList (G.labEdges gr2)

generatePatchLog :: (FruitMonitorGraph, InitialInputValues) -> Maybe [(Wire, Wire)]
generatePatchLog (gr, _) =
  let testCases = mkTestCases gr
      nodeMap = M.fromList $ swap <$> G.labNodes gr
      search = traverse (patchCircuitForTestCase nodeMap) (reorderEnds testCases)
   in concat <$> listToMaybe (evalStateT search gr)

solve1 :: InitialState -> Maybe Int
solve1 initialState = do
  (gr, initialValues) <- mkFruitMonitorGraph initialState
  fruitMonitor <- mkFruitMonitor gr
  runReader (runMaybeT $ sinks fruitMonitor) initialValues

solve2 :: InitialState -> Maybe String
solve2 = mkFruitMonitorGraph >=> generatePatchLog >=> swappedWires
  where
    swappedWires :: [(Wire, Wire)] -> Maybe String
    swappedWires = Just . L.intercalate "," . L.sort . concatMap (\(w1, w2) -> [w1, w2])

main :: IO ()
main = do
  (fruitMonitorFile : _) <- getArgs
  fruitMonitorConfig <- parseFileWith readFruitMonitor fruitMonitorFile

  case solve1 fruitMonitorConfig of
    Just value -> putStrLn $ "Monitor output: " ++ show value
    Nothing -> putStrLn "Could not compute monitor output."

  case solve2 fruitMonitorConfig of
    Just wires -> putStrLn $ "Swapped wires: " ++ show wires
    Nothing -> putStrLn "Could not patch circuit."
