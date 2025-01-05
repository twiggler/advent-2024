{-# LANGUAGE TupleSections #-}

import Data.Char
import Data.Functor
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.IntMap.Strict qualified as Map
import Data.IntSet qualified as Set
import Data.List
import Data.Maybe
import Parsing
import System.Environment
import Text.ParserCombinators.ReadP

data SleighManual = SleighManual [(Int, Int)] [[Int]]

pageNumber :: ReadP Int
pageNumber = read <$> munch1 isDigit

eol :: ReadP ()
eol = char '\n' $> ()

pageOrderingRule :: ReadP (Int, Int)
pageOrderingRule = (,) <$> pageNumber <*> (char '|' *> pageNumber)

pageUpdate :: ReadP [Int]
pageUpdate = sepBy1 pageNumber (char ',')

sleighManual :: ReadP SleighManual
sleighManual = SleighManual <$> pageOrderingRules <*> pageUpdates <* eol <* eof
  where
    pageOrderingRules = sepBy pageOrderingRule eol <* eol <* eol
    pageUpdates = sepBy pageUpdate eol

isTopologicallySorted :: [(Int, Int)] -> [Int] -> Bool
isTopologicallySorted edges' xs = all ordered edges'
  where
    ranking = Map.fromList $ zip xs [0 ..] :: Map.IntMap Int
    ordered (x, y) = fromMaybe True ((<) <$> Map.lookup x ranking <*> Map.lookup y ranking)

middle :: [Int] -> Maybe Int
middle [] = Nothing
middle zs = go zs zs
  where
    go (_ : _ : xs) ~(_ : ys) = go xs ys
    go _ (y : _) = Just y

orderUpdate :: [(Int, Int)] -> [Int] -> [Int]
orderUpdate rules ordering = topsort graph
  where
    nodes' = (,()) <$> ordering
    activeRules =
      let pageSet = Set.fromList ordering
       in filter (\(x, y) -> Set.member x pageSet && Set.member y pageSet) rules
    edges' = (\(x, y) -> (x, y, ())) <$> activeRules
    graph = mkGraph nodes' edges' :: Gr () ()

sumOfUpdates :: SleighManual -> (Int, Int)
sumOfUpdates (SleighManual rules updates) = (middleSum correctlyOrdered, middleSum reordered)
  where
    (correctlyOrdered, incorrectlyOrdered) = partition (isTopologicallySorted rules) updates
    reordered = orderUpdate rules <$> incorrectlyOrdered
    middleSum updates' = sum $ mapMaybe middle updates'

main :: IO ()
main = do
  (sleighManualFilePath : _) <- getArgs
  manual <- parseFileWith sleighManual sleighManualFilePath
  let (sumOfOrderedUpdates, sumOfReorderedUpdates) = sumOfUpdates manual
  putStrLn $ "Sum of ordered updates equals " ++ show sumOfOrderedUpdates
  putStrLn $ "Sum of reordered updates equals " ++ show sumOfReorderedUpdates
