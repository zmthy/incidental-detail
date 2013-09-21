--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.

module Types where

-- | Source
import Hastwix
import Vectwix

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Polygon types
cuboid     = "polyCube"
sphere     = "polySphere"
cylinder   = "polyCylinder"

--------------------------------------------------------------------------------
-- | A pair of points representing a 3D position and normal vector.
type Location = (Point, Point)

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
-- | two pie are.
twopi = 3.1415926 * 2

--------------------------------------------------------------------------------
-- | Linear interpolation
lerp :: Double -> Double -> Double -> Double
lerp a b t = a + (b - a) * t

--------------------------------------------------------------------------------
-- | Gives a point on a the surface of a sphere give the incident and 
-- | azimuth angles.
locOnSphere :: Double -> Double -> Double -> Location
locOnSphere r inc azi = ((x, y, z), (x, y, z))
    where x = r * (sin inc) * (cos azi)
          y = r * (sin inc) * (sin azi)
          z = r * (cos inc)

--------------------------------------------------------------------------------
-- | Generates a sphere represented by an array of locations
genSphere :: Double -> Int -> [Location]
genSphere r d = concat $ map (\i -> loop (twopi / (fromIntegral d) * i)) [0 .. (fromIntegral d) - 1]
    where loop a = map (\i -> locOnSphere r (twopi / (fromIntegral d) * i) a) [0 .. (fromIntegral d) - 1]

--------------------------------------------------------------------------------
-- | Generates a plane represented by an array of locations.
genPlane :: Int -> [Point]
genPlane d = concat $ map (\i -> line (lerp 0 1 (1.0 / (fromIntegral d) * i))) [0 .. (fromIntegral d)]
    where line h = map (\i -> (lerp 0 1 (1.0 / (fromIntegral d) * i), h, 0)) [0 .. (fromIntegral d)]

--------------------------------------------------------------------------------
-- | Transforms each point in an array of locations with the given matrix.
transformLocs :: [Location] -> Matrix Double -> [Location]
transformLocs locs tForm = map (\(p, o) -> (asVec (dot tForm (asCol p)), asVec (dot tForm (asCol o)))) locs

--------------------------------------------------------------------------------
-- | Updates locations based on a polygon's transform.
updateLocs :: Polygon -> Polygon
updateLocs p = Polygon locs (transform p) (upVector p) (rightVector p) (pType p)
    where locs = transformLocs (locations p) (transform p)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------