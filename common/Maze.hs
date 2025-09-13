module Maze
  ( Maze (..),
    MazeArray,
    Coord2,
    Cell (..),
    loadMaze,
    upsert,
    neighbors,
    neighbors1,
    isWall,
  )
where

import Control.Monad (join)
import Data.Array (Array, array)
import Data.Array.IArray ((!?))
import Data.Bifunctor (second)
import Data.Functor (($>))
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as PQ
import Parsing (grid, parseFileWithSafe)
import Text.ParserCombinators.ReadP (ReadP, eof)
import Text.ParserCombinators.ReadP qualified as P

type MazeArray = Array Coord2 Cell

data Maze = Maze
  { cells :: MazeArray,
    start :: Coord2,
    end :: Coord2
  }
  deriving (Show, Eq)

type Coord2 = (Int, Int)

data Cell = Empty | Wall deriving (Eq, Show)

data RawCell = REmpty | RWall | Start | End deriving (Eq)

mazeParser :: ReadP (Int, [[RawCell]])
mazeParser =
  grid
    ( P.choice
        [ P.char '.' $> REmpty,
          P.char '#' $> RWall,
          P.char 'S' $> Start,
          P.char 'E' $> End
        ]
    )

mkMaze :: Int -> [[RawCell]] -> Either String Maze
mkMaze dim rawCells = do
  let coords = [(x, y) | y <- [0 .. dim - 1], x <- [0 .. dim - 1]]
      assocs = coords `zip` join rawCells
      startPositions = fst <$> filter ((== Start) . snd) assocs
      endPositions = fst <$> filter ((== End) . snd) assocs

  startPos <- case startPositions of
    [s] -> Right s
    _ -> Left "Maze must have exactly one start (S) position."

  endPos <- case endPositions of
    [e] -> Right e
    _ -> Left "Maze must have exactly one end (E) position."

  let mazeArray = array ((0, 0), (dim - 1, dim - 1)) (second toCell <$> assocs)
  return $ Maze mazeArray startPos endPos
  where
    toCell REmpty = Empty
    toCell RWall = Wall
    toCell Start = Empty
    toCell End = Empty

loadMaze :: FilePath -> IO (Either String Maze)
loadMaze path = do
  rawMaze <- parseFileWithSafe (mazeParser <* eof) path
  let rawMazeEither = maybe (Left "Maze parsing failed") Right rawMaze
  return $ rawMazeEither >>= uncurry mkMaze

-- TODO: move to Search module.
upsert :: (Ord k, Ord p) => (p, v) -> ((p, v) -> (p, v)) -> k -> OrdPSQ k p v -> OrdPSQ k p v
upsert initial update key queue = snd $ PQ.alter f key queue
  where
    f = ((),) . Just . maybe initial update

neighbors :: Int -> Coord2 -> [Coord2]
neighbors r (x, y) =
  [(x + dx * r, y + dy * r) | dx <- [-1 .. 1], dy <- [-1 .. 1], abs dx + abs dy == 1]

neighbors1 :: Coord2 -> [Coord2]
neighbors1 = neighbors 1

isWall :: MazeArray -> Coord2 -> Bool
isWall maze coord = case maze !? coord of
  Just Wall -> True
  Just Empty -> False
  Nothing -> True
