module Types where


------------------------------------------------------------------------------
--import Data.Matrix


------------------------------------------------------------------------------
-- | A point in 3D space.
type Point = (Double, Double, Double)

px :: Point -> Double
px (x, _, _) = x

py :: Point -> Double
py (_, y, _) = y

pz :: Point -> Double
pz (_, _, z) = z

------------------------------------------------------------------------------
-- | A 3D transform.
type Transform = (Point, Point, Point)

tt :: Transform -> Point
tt (t, _, _) = t

tr :: Transform -> Point
tr (_, r, _) = r

ts :: Transform -> Point
ts (_, _, s) = s


------------------------------------------------------------------------------
data Polygon = Polygon
    { vertices    :: [Point]
    , faces       :: [(Int, Int, Int)]
    , upVector    :: Point
    , rightVector :: Point
    }


--data Scene
--    = Polygons [Polygon]
--    | Scenes [Scene]
--    | Transformation (Matrix Double) Scene