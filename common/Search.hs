module Search (bfs, prune, path, steps) where

import Control.Monad (join)
import Data.Foldable (foldl')
import Data.HashMap.Lazy (HashMap, (!))
import Data.HashMap.Lazy qualified as HL
import Data.Hashable (Hashable)
import Data.Sequence (Seq ((:<|)), (><))
import Data.Sequence qualified as S

bfs :: (Eq n, Hashable n) => (n -> [n]) -> n -> HashMap n (Maybe n)
bfs next start' = go (S.singleton start') (HL.singleton start' Nothing)
  where
    go (parent :<| rest) visited =
      let neighbors' = filter (not . flip HL.member visited) (next parent)
          visited' = foldl' (\m node -> HL.insert node (Just parent) m) visited neighbors'
          queue' = rest >< S.fromList neighbors'
       in go queue' visited'
    go S.Empty visited = visited

prune :: (n -> [n]) -> (n -> Bool) -> (n -> [n])
next `prune` pred' = filter (not . pred') <$> next

path :: (Eq n, Hashable n) => HashMap n (Maybe n) -> n -> [n]
path spt = reverse . go
  where
    go node = case join $ HL.lookup node spt of
      (Just parent) -> node : go parent
      _ -> [node]

-- | Compute the number of steps to each node in the shortest path tree.
steps :: (Eq n, Hashable n) => HashMap n (Maybe n) -> HashMap n Int
steps spt = steps'
  where
    steps' = stepCount <$> spt

    stepCount (Just n) = 1 + steps' ! n
    stepCount Nothing = 0
