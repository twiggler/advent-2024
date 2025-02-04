module Cursor
  ( Cursor (..),
    Grid,
    moveFwd,
    moveBwd,
    mapRows,
    extract,
    extractGrid,
  )
where

import Data.Functor.Compose
import Data.List.Infinite ( Infinite(..) )

data Cursor a = Cursor {bwd :: Infinite a, cur :: a, fwd :: Infinite a} deriving (Functor)

instance Applicative Cursor where
  pure a = Cursor (pure a) a (pure a)
  Cursor b1 c1 f1 <*> Cursor b2 c2 f2 = Cursor (b1 <*> b2) (c1 c2) (f1 <*> f2)

moveFwd, moveBwd :: Cursor a -> Cursor a
moveFwd (Cursor bwd' cur' (fwd' :< next')) = Cursor (cur' :< bwd') fwd' next'
moveBwd (Cursor (bwd' :< prev') cur' fwd') = Cursor prev' bwd' (cur' :< fwd')

-- We do not need full Comonad implementation
extract :: Cursor a -> a
extract (Cursor _ cur' _) = cur'

type Grid a = Compose Cursor Cursor a

mapRows :: (Cursor a -> Cursor a) -> Grid a -> Grid a
mapRows f (Compose (Cursor bwd' cur' fwd')) = Compose $ Cursor (f <$> bwd') (f cur') (f <$> fwd')

extractGrid :: Grid a -> a
extractGrid (Compose (Cursor _ cur' _)) = extract cur'
