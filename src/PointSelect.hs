------------------------------------------------------------------------------
module PointSelect
    (
    -- * PointSelect
      PointSelect (..)
    , toPoints

    -- * AxisSelect
    , AxisSelect
    , xAxis
    , yAxis
    , zAxis
    ) where

------------------------------------------------------------------------------
import Data.Monoid

import Selection


------------------------------------------------------------------------------
data PointSelect
    = CubeFaces AxisSelect
    | CylinderLoop Int Double
    deriving (Show)


------------------------------------------------------------------------------
data AxisSelect = AxisSelect
    { isX :: Bool
    , isY :: Bool
    , isZ :: Bool
    } deriving (Show)

instance Monoid AxisSelect where
    mempty = AxisSelect False False False
    mappend a b = AxisSelect
      { isX = isX a || isX b
      , isY = isY a || isY b
      , isZ = isZ a || isZ b
      }


------------------------------------------------------------------------------
xAxis :: AxisSelect
xAxis = AxisSelect True False False


------------------------------------------------------------------------------
yAxis :: AxisSelect
yAxis = AxisSelect False True False


------------------------------------------------------------------------------
zAxis :: AxisSelect
zAxis = AxisSelect False False True


------------------------------------------------------------------------------
toPoints :: PointSelect -> [Point]
toPoints ps = case ps of
    CubeFaces axes   -> boundingBox axes
    CylinderLoop c h -> horiLoop c h


------------------------------------------------------------------------------
boundingBox :: AxisSelect -> [Point]
boundingBox _ = []

------------------------------------------------------------------------------
horiLoop :: Int -> Double -> [Point]
horiLoop _ _ = []

