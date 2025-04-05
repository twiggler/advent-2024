module Cursor
  ( Cursor (..),
    Grid (..),
    PaddedGrid,
    moveFwd,
    moveBwd,
    moveCursor,
    moveGrid,
    mkGrid,
    toPaddedGrid,
    toMatrix,
    getCursor,
    toDir,
    Dir (..),
    Dir2,
    CardinalDir (..),
  )
where

import Control.Comonad
import Data.Distributive
import Data.Functor.Compose
import Data.List.Infinite (Infinite (..))
import Data.List.Infinite qualified as I
import Data.Maybe (isJust, fromJust)

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
toMatrix = toList (not.null) . fmap (fmap fromJust . toList isJust)  . getCursor
  where
    toList p (Cursor bwd' cur' fwd') = reverse (I.takeWhile p bwd') ++ I.takeWhile p (cur':<fwd')


data Dir = B | Z | F deriving (Show, Eq)

type Dir2 = (Dir, Dir)

data CardinalDir = North | East | South | West deriving (Eq, Show, Ord)

toDir :: CardinalDir -> Dir2
toDir North = (Z, B)
toDir East = (F, Z)
toDir South = (Z, F)
toDir West = (B, Z)

moveCursor :: Dir -> Cursor a -> Cursor a
moveCursor B = moveBwd
moveCursor Z = id
moveCursor F = moveFwd

moveGrid :: Dir2 -> Grid a -> Grid a
moveGrid (h, v) = mkGrid . moveCursor v . fmap (moveCursor h) . getCursor
