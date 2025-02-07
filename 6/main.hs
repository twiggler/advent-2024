import Control.Monad (MonadPlus (mzero), (<=<))
import Control.Monad.State (State, evalState)
import Control.Monad.State qualified as St
import Control.Monad.Trans.Maybe
import Cursor
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Foldable (asum)
import Data.Functor
import Data.Functor.Compose
import Data.List (find)
import Data.List.Infinite (Infinite ((:<)))
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List.NonEmpty qualified as Ne (last)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Parsing
import System.Environment
import Text.ParserCombinators.ReadP
import Control.Monad.Extra (unfoldM, liftMaybe)

data Cell = Empty | Obstruction | Start deriving (Show, Eq, Ord)

data TaggedCell = TaggedCell
  { cell :: Cell,
    tag :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

data CardinalDir = North | East | South | West deriving (Eq, Show, Ord)

{-
The world is modelled as an infinite grid, with a padding of Nothings.
The guard position is not stored explicitly, but is the focus of the cursor composed with a cursor.
The outer cursor represents the rows, the inner cursor represents the columns.

Downsides of this approach: 
- Moving the guard to a specific position, for example the start position, is clunky.
- We need to compose the Grid with another data structure *World* to store the current direction anyway.
- I suspect the performance is not optimal. This would not have been a problem if not for part 2.

Alternatives:
- Use a 2d array with fixed size.
- Store every cell type in a dedicated set. 
-}

data World = World
  { grid :: Grid (Maybe TaggedCell),
    __ :: CardinalDir
  }

data Guard = Guard
  { position :: (Int, Int),
    direction :: CardinalDir
  }
  deriving (Eq, Ord, Show)

guard :: World -> Maybe Guard
guard (World grid' dir') = do
  cell' <- extractGrid grid'
  return (Guard (tag cell') dir')

turnRight :: CardinalDir -> CardinalDir
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

makeWorld :: [[Cell]] -> World
makeWorld cells' = World (moveToStart (startTag tagged) $ toGrid tagged) North
  where
    tagged = tagCells cells'
    startTag = maybe (0, 0) tag . asum . fmap (find (\it -> cell it == Start))
    moveToStart (x, y) = advance y South . advance x East
    advance n dir' grid' = iterate (move2Guard dir') grid' !! n

-- Intuition: guard stays still, world moves
move2Guard :: CardinalDir -> Grid (Maybe TaggedCell) -> Grid (Maybe TaggedCell)
move2Guard d = Compose . moveVer d . getCompose . mapRows (moveHor d)
  where
    moveHor North = id
    moveHor East = moveFwd
    moveHor South = id
    moveHor West = moveBwd
    moveVer North = moveBwd
    moveVer East = id
    moveVer South = moveFwd
    moveVer West = id

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

toGrid :: [[TaggedCell]] -> Grid (Maybe TaggedCell)
toGrid = Compose . toCur (pure Nothing) . fmap (toCur Nothing . fmap Just)
  where
    toCur padding [] = Cursor (pure padding) padding (pure padding)
    toCur padding (a : as) = Cursor (pure padding) a (listToStream padding as)
    listToStream padding = foldr (:<) (pure padding)

-- Optimization: instead of moving a single cell at a time, we can raycast to the next obstacle. 
step :: World -> Maybe World
step (World grid' dir') = find isEmptyCell $ zipWith World candidateMoves candidateDirs
  where
    candidateDirs = take 4 (iterate turnRight dir')
    candidateMoves = flip move2Guard grid' <$> candidateDirs
    isEmptyCell = (Just Obstruction /=) . fmap cell . extractGrid . grid

patrol :: World -> NonEmpty World
-- Track the guard position and direction using a Set to terminate at cycles.
-- The cycle check could also be done outside of simulate.
-- This would make it easier to to determine the cause of termination. 
patrol world = world :| evalState (unfoldM (runMaybeT . go) world) Set.empty
  where
    go :: World -> MaybeT (State (Set Guard)) (World, World)
    go w0 = do
      w1 <- liftMaybe $ step w0
      guard' <- liftMaybe $ guard w1

      seen <- St.get
      if guard' `Set.member` seen
        then mzero
        else do
          St.put $ Set.insert guard' seen
          return (w1, w1)

solve1 :: NonEmpty World -> Int
solve1 = length . nubOrd . fmap position . mapMaybe guard . toList

solve2 :: NonEmpty World -> Int
solve2 =  length . mapMaybe tryCaptureGuard . obstaclePositions
  where
    obstaclePositions = nubOrdOn (fmap position . guard) . tail . toList
    tryCaptureGuard = guard <=< step . Ne.last . patrol . setupWorld  
    setupWorld (World g d) =
      let d' = turnRight $ turnRight d
          g' = move2Guard d' $ placeObstacle g
      in World g' d
    placeObstacle = updateGrid $ fmap (\tc -> tc { cell = Obstruction })

main :: IO ()
main = do
  (labLayoutFilePath : _) <- getArgs
  lab' <- parseFileWith lab labLayoutFilePath
  let path = patrol $ makeWorld lab'
      distinctPositions = solve1 path
      obstaclesWithLoop = solve2 path
  putStrLn $ "Number of distinct positions visited equals " ++ show distinctPositions
  putStrLn $ "Number of obstacles causing a loop equals  " ++ show obstaclesWithLoop
  