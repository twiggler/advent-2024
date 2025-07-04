module Cursor
  ( Cursor (..),
    Grid (..),
    PaddedGrid,
    moveFwd,
    moveBwd,
    moveCursor,
    moveGrid,
    neighbors,
    mkGrid,
    toPaddedGrid,
    toMatrix,
    getCursor,
    toDir,
    reverseDir,
    reverseDir2,
    Dir (..),
    Dir2,
    CardinalDir (..),
    cardinalDirs,
    toVector,
    toVector2,
    FocussedCursor,
    FocussedGrid,
    toFocussedGrid,
    fromFocussedGrid,
    focus
  )
where

import Control.Comonad ( Comonad(extract, duplicate) )
import Data.Distributive ( Distributive(distribute) )
import Data.Functor.Compose ( Compose(..) )
import Data.List.Infinite (Infinite (..))
import Data.List.Infinite qualified as I (iterate, transpose, takeWhile)
import Data.Maybe ( fromJust, isJust, mapMaybe )

data Cursor a = Cursor {bwd :: Infinite a, cur :: a, fwd :: Infinite a} deriving (Functor)

instance Applicative Cursor where
  pure a = Cursor (pure a) a (pure a)
  Cursor b1 c1 f1 <*> Cursor b2 c2 f2 = Cursor (b1 <*> b2) (c1 c2) (f1 <*> f2)

instance Comonad Cursor where
  extract (Cursor _ cur' _) = cur'
  duplicate c = Cursor (I.iterate moveBwd (moveBwd c)) c (I.iterate moveFwd (moveFwd c))

instance Distributive Cursor where
  distribute fCur = Cursor (I.transpose (bwd <$> fCur)) (cur <$> fCur) (I.transpose (fwd <$> fCur))

moveFwd, moveBwd :: Cursor a -> Cursor a
moveFwd (Cursor bwd' cur' (fwd' :< next')) = Cursor (cur' :< bwd') fwd' next'
moveBwd (Cursor (bwd' :< prev') cur' fwd') = Cursor prev' bwd' (cur' :< fwd')

newtype Grid a = Grid (Compose Cursor Cursor a)

instance Functor Grid where
  fmap f (Grid c) = Grid (fmap f c)

instance Comonad Grid where
  extract = extract . extract . getCursor

  -- NOT fmap duplicate . duplicate, see https://bartoszmilewski.com/2025/01/
  duplicate =
    fmap (Grid . Compose) . (Grid . Compose) . fmap distribute . duplicate . fmap duplicate . getCursor

mkGrid :: Cursor (Cursor a) -> Grid a
mkGrid = Grid . Compose

getCursor :: Grid a -> Cursor (Cursor a)
getCursor (Grid c) = getCompose c

type PaddedGrid a = Grid (Maybe a)

toPaddedGrid :: [[a]] -> PaddedGrid a
toPaddedGrid = mkGrid . toCur (pure Nothing) . fmap (toCur Nothing . fmap Just)
  where
    toCur padding [] = Cursor (pure padding) padding (pure padding)
    toCur padding (a : as) = Cursor (pure padding) a (rightPadded padding as)
    rightPadded padding = foldr (:<) (pure padding)

toMatrix :: PaddedGrid a -> [[a]]
toMatrix = toList (not . null) . fmap (fmap fromJust . toList isJust) . getCursor
  where
    toList p (Cursor bwd' cur' fwd') = reverse (I.takeWhile p bwd') ++ I.takeWhile p (cur' :< fwd')

data Dir = B | Z | F deriving (Show, Eq)

type Dir2 = (Dir, Dir)

data CardinalDir = North | East | South | West deriving (Eq, Show, Ord, Enum, Bounded)

-- TODO: Remove now CardinalDir is a Bounded Enum?
cardinalDirs :: [Dir2]
cardinalDirs = toDir <$> [minBound .. maxBound]

toDir :: CardinalDir -> Dir2
toDir North = (Z, B)
toDir East = (F, Z)
toDir South = (Z, F)
toDir West = (B, Z)

toVector :: Dir -> Int
toVector B = -1
toVector Z = 0
toVector F = 1

toVector2 :: Dir2 -> (Int, Int)
toVector2 (h, v) = (toVector h, toVector v)

reverseDir :: Dir -> Dir
reverseDir B = F
reverseDir Z = Z
reverseDir F = B

reverseDir2 :: Dir2 -> Dir2
reverseDir2 (h, v) = (reverseDir h, reverseDir v)

moveCursor :: Dir -> Cursor a -> Cursor a
moveCursor B = moveBwd
moveCursor Z = id
moveCursor F = moveFwd

moveGrid :: Dir2 -> Grid a -> Grid a
moveGrid (h, v) = mkGrid . moveCursor v . fmap (moveCursor h) . getCursor

-- TODO:: change return type to Maybe [(CardinalDir, a)]
neighbors :: PaddedGrid a -> Maybe [(Dir2, a)]
neighbors grid = case extract grid of
  Just _ -> Just $ mapMaybe move cardinalDirs
  Nothing -> Nothing
  where
    move d = fmap (d,) . extract $ moveGrid d grid

-- Cursor which is focussed on a Just value
data FocussedCursor a = FocussedCursor (Infinite (Maybe a)) a (Infinite (Maybe a))

-- A padded grid which is focussed on a Just value
data FocussedGrid a = FocussedGrid (Infinite (Cursor (Maybe a))) (FocussedCursor a) (Infinite (Cursor (Maybe a)))

toFocussedGrid :: PaddedGrid a -> Maybe (FocussedGrid a)
toFocussedGrid g =
  let (Cursor bwd' cur' fwd') = getCursor g
      (Cursor bwd'' cur'' fwd'') = cur'
   in case cur'' of
        Just c -> Just $ FocussedGrid bwd' (FocussedCursor bwd'' c fwd'') fwd'
        Nothing -> Nothing

fromFocussedGrid :: FocussedGrid a -> PaddedGrid a
fromFocussedGrid (FocussedGrid bwd' cur' fwd') =
  let (FocussedCursor bwd'' cur'' fwd'') = cur'
   in mkGrid $ Cursor bwd' (Cursor bwd'' (Just cur'') fwd'') fwd'

focus :: FocussedGrid a -> a
focus (FocussedGrid _ (FocussedCursor _ cur' _) _) = cur'
