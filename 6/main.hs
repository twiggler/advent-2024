import Cursor
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (asum)
import Data.Functor
import Data.Functor.Compose
import Data.List (find, unfoldr)
import Data.List.Infinite (Infinite ((:<)), cycle, head, tails, take)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Parsing
import System.Environment
import Text.ParserCombinators.ReadP
import Prelude hiding (cycle, head, take)

data Cell = Empty | Obstruction | Start deriving (Show, Eq, Ord)

data TaggedCell = TaggedCell
  { cell :: Cell,
    tag :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

data Dir = Neg | Zero | Pos deriving (Eq, Show)

type Dir2 = (Dir, Dir)

allDirs :: NonEmpty Dir2
allDirs = (Zero, Neg) :| [(Pos, Zero), (Zero, Pos), (Neg, Zero)]

data World = World
  { grid :: Grid (Maybe TaggedCell),
    __ :: Infinite Dir2
  }

makeWorld :: [[Cell]] -> World
makeWorld cells' = World (center (startTag tagged) $ toGrid tagged) (cycle allDirs)
  where
    tagged = tagCells cells'
    startTag = maybe (0, 0) tag . asum . fmap (find (\it -> cell it == Start))
    advance n dir grid' = iterate (move2Guard dir) grid' !! n
    center (x, y) = advance y (Zero, Pos) . advance x (Pos, Zero)

-- Intuition: guard stays still, world moves
move2Guard :: Dir2 -> Grid (Maybe TaggedCell) -> Grid (Maybe TaggedCell)
move2Guard (h, v) = Compose . moveGuard v . getCompose . mapRows (moveGuard h)
  where
    moveGuard Neg = moveBwd
    moveGuard Zero = id
    moveGuard Pos = moveFwd

validate :: Grid (Maybe TaggedCell) -> Bool
validate grid' = case extractGrid grid' of
  Just (TaggedCell Obstruction _) -> False
  _ -> True

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
toGrid = Compose . toCur (pure Nothing) . fmap (toCur Nothing) . maybes
  where
    maybes = (fmap . fmap) Just
    toCur padding [] = Cursor (pure padding) padding (pure padding)
    toCur padding (a : as) = Cursor (pure padding) a (listToStream padding as)
    listToStream padding = foldr (:<) (pure padding)

step :: World -> Maybe World
step (World grid' dirs') = find validWorld $ zipWith World candidateMoves candidateDirs
  where
    candidateDirs = take (length allDirs) $ tails dirs'
    candidateMoves = flip move2Guard grid' . head <$> candidateDirs
    validWorld = validate . grid

simulate :: World -> Int
simulate world = length $ nubOrd $ startPos $ unfoldr gen world
  where
    gen w0 = do
      w1 <- step w0
      pos <- extractWorld w1 
      return (pos, w1)
    startPos = maybe id (:) (extractWorld world)
    extractWorld = extractGrid . grid

main :: IO ()
main = do
  (labLayoutFilePath : _) <- getArgs
  lab' <- parseFileWith lab labLayoutFilePath
  let distinctPositions = simulate $ makeWorld lab'
  putStrLn $ "Number of distinct positions visited equals " ++ show distinctPositions
