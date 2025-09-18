import Control.Monad (MonadPlus (mzero), (<=<))
import Control.Monad.State (State, evalState)
import Control.Monad.State qualified as St
import Control.Monad.Trans.Maybe
import Cursor
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Foldable (asum)
import Data.Functor
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Parsing (eol, parseFileWith)
import System.Environment
import Text.ParserCombinators.ReadP
import Control.Monad.Extra (unfoldM, liftMaybe)
import Control.Comonad (extract)

data Cell = Empty | Obstruction | Start deriving (Show, Eq, Ord)

data TaggedCell = TaggedCell
  { cell :: Cell,
    tag :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

{-
UPDATE: reloaded.hs provides a more efficient solution.

The world is modelled as an infinite grid, with a padding of Nothings.
The guard position is not stored explicitly, but is the focus of the cursor composed with a cursor.
The outer cursor represents the rows, the inner cursor represents the columns.

Downsides of this approach: 
- Moving the guard to a specific position, for example the start position, is clunky.
- We need to compose the PaddedGrid with another data structure *World* to store the current direction anyway.
- I suspect the performance is not optimal. This would not have been a problem if not for part 2.

Alternatives:
- Use a 2d array with fixed size.
- Store every cell type in a dedicated set. 
-}

data World = World
  { grid :: PaddedGrid TaggedCell,
    __ :: CardinalDir
  }

data Guard = Guard
  { position :: (Int, Int),
    direction :: CardinalDir
  }
  deriving (Eq, Ord, Show)

guard :: World -> Maybe Guard
guard (World grid' dir') = do
  cell' <- extract grid'
  return (Guard (tag cell') dir')

turnRight :: CardinalDir -> CardinalDir
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

makeWorld :: [[Cell]] -> World
makeWorld cells' = World (moveToStart (startTag tagged) $ toPaddedGrid tagged) North
  where
    tagged = tagCells cells'
    startTag = maybe (0, 0) tag . asum . fmap (find (\it -> cell it == Start))
    moveToStart (x, y) = advance y South . advance x East
    advance n dir' grid' = iterate (moveGrid (toDir dir')) grid' !! n

lab :: ReadP [[Cell]]
lab = do
  firstRow <- manyTill cell' eol
  let width = length firstRow
  rest <- endBy (count width cell') eol <* eof
  return (firstRow : rest)
  where
    cell' = choice [char '.' $> Empty, char '#' $> Obstruction, char '^' $> Start]

tagCells :: [[Cell]] -> [[TaggedCell]]
tagCells = zipWith tagRow [0 ..]
  where
    tagRow y = zipWith (tagCell y) [0 ..]
    tagCell y x cell' = TaggedCell cell' (x, y)

-- Optimization: instead of moving a single cell at a time, we can raycast to the next obstacle. 
step :: World -> Maybe World
step (World grid' dir') = find isEmptyCell $ zipWith World candidateMoves candidateDirs
  where
    candidateDirs = take 4 (iterate turnRight dir')
    candidateMoves = flip moveGrid grid' . toDir <$> candidateDirs
    isEmptyCell = (Just Obstruction /=) . fmap cell . extract . grid

patrol :: World -> NonEmpty World
-- Track the guard position and direction using a Set to terminate at cycles.
-- The cycle check could also be done outside of simulate.
-- This would make it easier to to determine the cause of termination. 
patrol world = world :| evalState (unfoldM (runMaybeT . go) world) S.empty
  where
    go :: World -> MaybeT (State (Set Guard)) (World, World)
    go w0 = do
      w1 <- liftMaybe $ step w0
      guard' <- liftMaybe $ guard w1

      seen <- St.get
      if guard' `S.member` seen
        then mzero
        else do
          St.put $ S.insert guard' seen
          return (w1, w1)

solve1 :: NonEmpty World -> Int
solve1 = length . nubOrd . fmap position . mapMaybe guard . toList

solve2 :: NonEmpty World -> Int
solve2 =  length . mapMaybe tryCaptureGuard . obstaclePositions
  where
    obstaclePositions = nubOrdOn (fmap position . guard) . tail . toList
    tryCaptureGuard = guard <=< step . NE.last . patrol . setupWorld
    setupWorld (World g d) =
      let d' = turnRight $ turnRight d
          g' = moveGrid (toDir d') $ placeObstacle g
      in World g' d
    placeObstacle = updateGrid $ fmap (\tc -> tc { cell = Obstruction })
    updateGrid f = mkGrid . updateCursor (updateCursor f) . getCursor
    updateCursor f (Cursor bwd' cur' fwd') = Cursor bwd' (f cur') fwd'

main :: IO ()
main = do
  (labLayoutFilePath : _) <- getArgs
  lab' <- parseFileWith lab labLayoutFilePath
  let path = patrol $ makeWorld lab'
      distinctPositions = solve1 path
      obstaclesWithLoop = solve2 path
  putStrLn $ "Number of distinct positions visited equals " ++ show distinctPositions
  putStrLn $ "Number of obstacles causing a loop equals  " ++ show obstaclesWithLoop
