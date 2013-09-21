--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.

module Types where

-- | Source
import Hastwix
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Polygon types
cuboid     = "polyCube"
sphere     = "polySphere"
cylinder   = "polyCylinder"

--------------------------------------------------------------------------------
-- | Point, and a collection of functions because I used to OOP.
type Point = (Double, Double, Double)

px :: Point -> Double
px (x, _, _) = x

py :: Point -> Double
py (_, y, _) = y

pz :: Point -> Double
pz (_, _, z) = z

cross :: Point -> Point -> Point
cross a b = (x, y, z)
  where x = (py a) * (pz b) - (py b) * (pz a)
        y = (px b) * (pz a) - (px a) * (pz b)
        z = (px a) * (py b) - (py a) * (px b)

dotP :: Point -> Point -> Double
dotP a b = (px a) * (px b) + (py a) * (py b) + (pz a) * (pz b)

mag :: Point -> Double
mag v = sqrt $ px v ** 2 + py v ** 2 + pz v ** 2

norm :: Point -> Point
norm v = (x, y, z)
  where x = (px v) / m
        y = (py v) / m
        z = (pz v) / m
        m = mag v

--------------------------------------------------------------------------------
-- | A pair of points representing a 3D position and normal vector.
type Location = (Point, Point)

defaultLoc :: Location
defaultLoc = ((0, 0, 0), (0, 1, 0))

--------------------------------------------------------------------------------
-- | Stores a net of locations, a 3D transform, and a few other bits.
data Polygon = Polygon
    { locations   :: [Location]
    , transform   :: Matrix Double
    , upVector    :: Point
    , rightVector :: Point
    , pType       :: String
    }

--------------------------------------------------------------------------------
-- |
twopi = 3.1415926 * 2

--------------------------------------------------------------------------------
-- |
lerp :: Double -> Double -> Double -> Double
lerp a b t = a + (b - a) * t

--------------------------------------------------------------------------------
-- |
locOnSphere :: Double -> Double -> Double -> (Point, Point)
locOnSphere r inc azi = ((x, y, z), (x, y, z))
    where x = r * (sin inc) * (cos azi)
          y = r * (sin inc) * (sin azi)
          z = r * (cos inc)

--------------------------------------------------------------------------------
-- |
genSphere :: Double -> Int -> [(Point, Point)]
genSphere r d = concat $ map (\i -> loop (twopi / (fromIntegral d) * i)) [0 .. (fromIntegral d) - 1]
    where loop a = map (\i -> locOnSphere r (twopi / (fromIntegral d) * i) a) [0 .. (fromIntegral d) - 1]

--------------------------------------------------------------------------------
-- |
genPlane :: Int -> [Point]
genPlane d = concat $ map (\i -> line (lerp 0 1 (1.0 / (fromIntegral d) * i))) [0 .. (fromIntegral d)]
    where line h = map (\i -> (lerp 0 1 (1.0 / (fromIntegral d) * i), h, 0)) [0 .. (fromIntegral d)]

--------------------------------------------------------------------------------
-- |
transformLocs :: [(Point, Point)] -> Matrix Double -> [(Point, Point)]
transformLocs locs tForm = map (\(p, o) -> (asVec (dot tForm (asCol p)), asVec (dot tForm (asCol o)))) locs

--------------------------------------------------------------------------------
-- |
updateLocs :: Polygon -> Polygon
updateLocs p = Polygon locs (transform p) (upVector p) (rightVector p) (pType p)
    where locs = transformLocs (locations p) (transform p)

--------------------------------------------------------------------------------
-- |
locsAsArray :: [Point] -> [[Double]]
locsAsArray ps = concat $ map (\p -> arrFromPoint p) ps
    where arrFromPoint v = [[px v, py v, pz v]]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------