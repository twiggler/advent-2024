import Control.Monad (join, (>=>))
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Foldable (minimumBy)
import Data.List (nub)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Sequence (Seq)
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Parsing (readLines)
import System.Environment (getArgs)

type Coord2 = (Int, Int)

data KeyPad a = KeyPad
  { coordByButton :: Map a Coord2,
    startButton :: a,
    _gap :: Coord2
  }

data DirectionalButton = U | D | L | R | A
  deriving (Eq, Ord, Show)

type Choices a = [a]

type Directions = Choices [DirectionalButton]

numericKeypad :: KeyPad Char
numericKeypad =
  KeyPad
    ( M.fromList
        [ ('7', (0, 0)),
          ('8', (1, 0)),
          ('9', (2, 0)),
          ('4', (0, 1)),
          ('5', (1, 1)),
          ('6', (2, 1)),
          ('1', (0, 2)),
          ('2', (1, 2)),
          ('3', (2, 2)),
          ('0', (1, 3)),
          ('A', (2, 3))
        ]
    )
    'A'
    (0, 3) -- Important: gap must be on outside of keypad

directionalKeypad :: KeyPad DirectionalButton
directionalKeypad =
  KeyPad
    ( M.fromList
        [ (U, (1, 0)),
          (D, (1, 1)),
          (L, (0, 1)),
          (R, (2, 1)),
          (A, (2, 0))
        ]
    )
    A
    (0, 0) -- Important: gap must be on outside of keypad

buttonHops :: (Ord a) => KeyPad a -> [a] -> [(a, a)]
buttonHops keyPad buttons = zip (startButton keyPad : buttons) buttons

directionsForHop :: (Ord a) => KeyPad a -> (a, a) -> Maybe Directions
directionsForHop (KeyPad coordByButton' _ (gx, gy)) (source, dest) = do
  from <- M.lookup source coordByButton'
  to <- M.lookup dest coordByButton'
  return $ paths from to
  where
    paths (x, y) (x', y') =
      let (dx, dy) = (x' - x, y' - y)
          rowMajor = y /= gy || x' /= gx -- Assuming gap is on outside
          columnMajor = x /= gx || y' /= gy
       in directionsForOffset dx dy rowMajor columnMajor

directionsForOffset :: Int -> Int -> Bool -> Bool -> Directions
directionsForOffset dx dy rowMajor columnMajor =
  -- Skip interleaved horizontal and vertical movement because they are never optimal
  nub
    ( [horizontalCommands ++ verticalCommands ++ [A] | rowMajor]
        ++ [verticalCommands ++ horizontalCommands ++ [A] | columnMajor]
    )
  where
    horizontalCommands = replicate (abs dx) (if dx < 0 then L else R)
    verticalCommands = replicate (abs dy) (if dy < 0 then U else D)

directionsFromOffset' :: Int -> Int -> Directions
directionsFromOffset' dx dy = directionsForOffset dx dy True True

shortestChoice :: (Foldable t) => Choices (t DirectionalButton) -> Maybe (t DirectionalButton)
shortestChoice paths
  | null paths = Nothing
  | otherwise = Just $ minimumBy (comparing length) paths

complexity :: String -> Seq DirectionalButton -> Int
complexity code commandSequence = length commandSequence * (read . filter isDigit $ code)

-- Expands a code into a sequence of directional button presses.
-- The `levels` parameter represents the number of intermediary directional keypads.
expand :: Int -> String -> Maybe (Seq DirectionalButton)
expand levels = paths numericKeypad >=> foldMap (traverse (go levels) >=> shortestChoice)
  where
    go lvl
      | lvl > 1 =
          paths directionalKeypad >=> foldMap (descend (lvl - 1))
      | lvl == 1 =
          paths directionalKeypad >=> foldMap shortestChoice >=> Just . SQ.fromList
      | otherwise = Just . SQ.fromList

    paths :: (Ord a) => KeyPad a -> [a] -> Maybe [Directions]
    paths keypad = traverse (directionsForHop keypad) . buttonHops keypad

    descend :: Int -> Directions -> Maybe (Seq DirectionalButton)
    descend lvl' = traverse (\path -> join $ directions !? (lvl', path)) >=> shortestChoice

    -- Memoize directions for identical paths at each level.
    -- After the initial mapping from the numeric keypad, the entries of directions
    -- map the set of paths S to the smallest path in the cartesian product S1 x S2 ...
    directions =
      let (dimx, dimy) = bimap maximum maximum (unzip . M.elems . coordByButton $ directionalKeypad)
          keys =
            S.fromList
              [ (level, pattern')
                | dx <- [-dimx .. dimx],
                  dy <- [-dimy .. dimy],
                  level <- [1 .. levels - 1],
                  pattern' <- directionsFromOffset' dx dy
              ]
       in M.fromSet (uncurry go) keys

solve :: Int -> [String] -> Maybe Int
solve levels = fmap sum . traverse processCode
  where
    processCode code = complexity code <$> expand levels code

main :: IO ()
main = do
  [codeFilePath, levelsStr] <- getArgs
  codes <- readLines codeFilePath
  let sumComplexities1 = solve (read levelsStr) codes
  case sumComplexities1 of
    Nothing -> putStrLn "No valid complexity scores found."
    Just total -> putStrLn $ "Sum of complexity scores equals " ++ show total
