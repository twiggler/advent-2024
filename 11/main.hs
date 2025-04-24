{-# LANGUAGE DataKinds #-}

import Data.Finite (getFinite, packFinite)
import Data.Vector.Sized (Vector, generate, index)
import Math.NumberTheory.Logarithms (integerLog10')
import Parsing
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

data Stone = Stone Integer EvolutionTree

data EvolutionTree = Single Stone | Split Stone Stone

type EvolveInto = Either Integer (Integer, Integer)

readStoneArrangement :: ReadP [Integer]
readStoneArrangement = sepBy number (char ' ') <* eol <* eof

evolveStone :: Integer -> EvolveInto
evolveStone v
  | v == 0 = Left 1
  | even nDigits =
      let (a, b) = v `quotRem` (10 ^ (nDigits `div` 2))
       in Right (a, b)
  | otherwise = Left (v * 2024)
  where
    nDigits = integerLog10' v + 1 -- v /= 0

unfold :: (Integer -> EvolveInto) -> Integer -> EvolutionTree
unfold f = step
  where
    step = go . f
    go (Left a) = Single (Stone a (step a))
    go (Right (a, b)) = Split (Stone a (step a)) (Stone b (step b))

-- Tightly coupled to evolveStone because memoization is tailored for it,
-- and there is little point in separate the memoization (by using fix)
stonesPerLevel :: Integer -> [Integer]
stonesPerLevel = (1 :) . go . unfold evolveStone
  where
    go (Single (Stone v t)) = 0 : branch v t
    go (Split (Stone v1 t1) (Stone v2 t2)) = 1 : zipWith (+) (branch v1 t1) (branch v2 t2)

    branch v t = case packFinite v of
      Just v' -> table `index` v'
      Nothing -> go t

    -- Exploit that unfolding evolveStone leads to isomorphic subtrees with identical labeling 
    table :: Vector 1000 [Integer]
    table = generate (go . unfold evolveStone . getFinite)

blink :: Int -> [Integer] -> Integer
blink n = sum . fmap (sum . take (n + 1) . stonesPerLevel)

main :: IO ()
main = do
  (stoneArrangementPath : numberOfBlinksStr : _) <- getArgs
  let numberOfBlinks = read numberOfBlinksStr :: Int
  stoneArrangement <- parseFileWith readStoneArrangement stoneArrangementPath
  let numberOfStones = blink numberOfBlinks stoneArrangement
  putStrLn $ "Number of stones after blinking " ++ show numberOfBlinks ++ " times equals " ++ show numberOfStones
