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
type PolyType = String
type Poly = (PolyType, String, Transform)
type PolySet = [Poly]


--------------------------------------------------------------------------------
-- | A point in 3D space.
type Point = (Double, Double, Double)

px :: Point -> Double
px (x, _, _) = x

py :: Point -> Double
py (_, y, _) = y

pz :: Point -> Double
pz (_, _, z) = z

--------------------------------------------------------------------------------
-- | A 3D transform.
type Transform = (Point, Point, Point)

tt :: Transform -> Point
tt (t, _, _) = t

tr :: Transform -> Point
tr (_, r, _) = r

ts :: Transform -> Point
ts (_, _, s) = s


--------------------------------------------------------------------------------
-- |
data Polygon = Polygon
    { locations   :: [(Point, Point)]
    , transform   :: Matrix Double
    , upVector    :: Point
    , rightVector :: Point
    , pType       :: String
    }

--------------------------------------------------------------------------------
-- |
crossProduct :: Point -> Point -> Point
crossProduct a b = (x, y, z)
    where x = (py a) * (pz b) - (py b) * (pz a)
          y = (px b) * (pz a) - (px a) * (pz b)
          z = (px a) * (py b) - (py a) * (px b)

--------------------------------------------------------------------------------
-- |
dotProduct :: Point -> Point -> Double
dotProduct a b = (px a) * (px b) + (py a) * (py b) + (pz a) * (pz b)

--------------------------------------------------------------------------------
-- |
normalizeVec :: Point -> Point
normalizeVec v = (x, y, z)
    where x = (px v) / length
          y = (py v) / length
          z = (pz v) / length
          length = sqrt $ px v ** 2 + py v ** 2 + pz v ** 2

--------------------------------------------------------------------------------
-- |
applyParentTransform :: Polygon -> Polygon -> Polygon
applyParentTransform p c = Polygon (locations c) tForm (upVector c) (rightVector c) (pType c)
    where tForm = dot (transform p) (transform c)

--------------------------------------------------------------------------------
-- |
toColMtx :: Point -> Matrix Double
toColMtx p = Matrix [px p, py p, pz p, 1] (4, 1)

--------------------------------------------------------------------------------
-- |
toVec :: Matrix Double -> Point
toVec (Matrix [x, y, z, _] shape) = (x, y, z)

--------------------------------------------------------------------------------
-- |
getAngle :: Point -> Point -> Double
getAngle vA vB = acos $ dotProduct (normalizeVec vA) (normalizeVec vB)

--------------------------------------------------------------------------------
-- |
getAxis :: Point -> Point -> Point
getAxis vA vB
    | (crossProduct vA vB) == (0, 0, 0) = (1, 0, 0)
    | otherwise = normalizeVec $ crossProduct vA vB

--------------------------------------------------------------------------------
-- |
skewAxis :: Point -> Matrix Double
skewAxis a = Matrix [ 0, pz a, -py a,
                      -pz a, 0, px a,
                      py a, -px a, 0 ] (3, 3)

--------------------------------------------------------------------------------
-- |
rotationBetweenVecs :: Double -> Point -> Matrix Double
rotationBetweenVecs angle axis = Matrix [ rotMatrix !! 0, rotMatrix !! 1, rotMatrix !! 2, 0, 
                                          rotMatrix !! 3, rotMatrix !! 4, rotMatrix !! 5, 0,
                                          rotMatrix !! 6, rotMatrix !! 7, rotMatrix !! 8, 0, 
                                          0, 0, 0, 1 ] (4, 4)
    where rotMatrix   = components $ add eye (add (mult sinScalar skewed) (mult cosScalar skewSquared))
          eye         = (identity 3)
          sinScalar   = sin angle
          cosScalar   = 1 - (cos angle)
          skewSquared = (dot skewed skewed)
          skewed      = skewAxis axis

--------------------------------------------------------------------------------
-- |
orientMtx :: Point -> Point -> Matrix Double
orientMtx a b = rotationBetweenVecs (getAngle  a b) (getAxis a b)

--------------------------------------------------------------------------------
-- |
moveMtx :: Point -> Matrix Double
moveMtx pos = Matrix [ 1, 0, 0, px pos,
                       0, 1, 0, py pos,
                       0, 0, 1, pz pos, 
                       0, 0, 0, 1 ] (4, 4)

--------------------------------------------------------------------------------
-- |
dickMtx :: Double -> Matrix Double
dickMtx a = Matrix [ cos a, -sin a, 0, 0,
                     sin a, cos a, 0, 0,
                     0, 0, 1, 0, 
                     0, 0, 0, 1 ] (4, 4)

--------------------------------------------------------------------------------
-- |
scaleMtx :: Point -> Matrix Double
scaleMtx scale = Matrix [ px scale, 0, 0, 0,
                          0, py scale, 0, 0,
                          0, 0, pz scale, 0,
                          0, 0, 0, 1 ] (4, 4)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------