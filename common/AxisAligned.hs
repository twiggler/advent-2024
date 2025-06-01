module AxisAligned
  ( Coord2,
    Bounds,
    AlignedAxisMaps,
    castRay,
    makeAlignedAxesMaps,
    addCoord
  )
where

import Cursor (CardinalDir (..))
import Data.Bifunctor qualified as BF (second)
import Data.Functor ((<&>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM (fromListWith, insertWith, lookup)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS (lookupGT, lookupLT, singleton, union)
import Data.Tuple (swap)

type Coord2 = (Int, Int)

type Bounds = Coord2

-- Data.IxSet.Typed is a good fit, except it doesn't support lookupLT and lookupGT
type AlignedAxisMaps = (IntMap IntSet, IntMap IntSet)

makeAlignedAxesMaps :: [Coord2] -> AlignedAxisMaps
makeAlignedAxesMaps cells =
  let xByY = IM.fromListWith IS.union (keyValue swap cells)
      yByX = IM.fromListWith IS.union (keyValue id cells)
   in (xByY, yByX)
  where
    keyValue f = fmap (BF.second IS.singleton . f)

castRay :: AlignedAxisMaps -> Coord2 -> CardinalDir -> Maybe Coord2
castRay (xByY, yByX) (x, y) dir = case dir of
  North -> IM.lookup x yByX >>= IS.lookupLT y <&> (x,)
  East -> IM.lookup y xByY >>= IS.lookupGT x <&> (,y)
  South -> IM.lookup x yByX >>= IS.lookupGT y <&> (x,)
  West -> IM.lookup y xByY >>= IS.lookupLT x <&> (,y)

addCoord :: Coord2 -> AlignedAxisMaps -> AlignedAxisMaps
addCoord (x, y) (xByY, yByX) =
  let xByY' = IM.insertWith IS.union y (IS.singleton x) xByY
      yByX' = IM.insertWith IS.union x (IS.singleton y) yByX
   in (xByY', yByX')
