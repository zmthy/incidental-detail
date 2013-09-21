module Types where

import Hastwix

------------------------------------------------------------------------------
--import Data.Matrix

------------------------------------------------------------------------------
-- | Polygon types
cuboid     = "polyCube"
sphere     = "polySphere"
cylinder   = "polyCylinder"
type PolyType = String
type Poly = (PolyType, String, Transform)
type PolySet = [Poly]


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
    { locations   :: [Point]
    , transform   :: Matrix Double
    , upVector    :: Point
    , rightVector :: Point
    , pType       :: String
    }

crossProduct :: Point -> Point -> Point
crossProduct a b = (x, y, z)
    where x = (py a) * (pz b) - (py b) * (pz a)
          y = (px b) * (pz a) - (px a) * (pz b)
          z = (px a) * (py b) - (py a) * (px b)

dotProduct :: Point -> Point -> Double
dotProduct a b = (px a) * (px b) + (pz a) * (pz b) + (pz a) * (pz b)

normalizeVec :: Point -> Point
normalizeVec v = (x, y, z)
    where x = (px v) / length
          y = (py v) / length
          z = (pz v) / length
          length = sqrt $ px v ** 2 + py v ** 2 + pz v ** 2

applyParentTransform :: Polygon -> Polygon -> Polygon
applyParentTransform p c = Polygon (locations c) tForm (upVector c) (rightVector c) (pType c)
    where tForm = dot (transform p) (transform c)

toColMtx :: Point -> Matrix Double
toColMtx p = Matrix [px p, py p, pz p, 1] (4, 1)

toVec :: Matrix Double -> Point
toVec (Matrix [x, y, z, _] shape) = (x, y, z)

--translatePoly :: Point -> Polygon -> Polygon
--translatePoly t p = Polygon (locations p) tForm (upVector p) (rightVector p)
--    where tForm = dot translate (transform p)
--          translate = Matrix [ 1, 0, 0, px t,
--                               0, 1, 0, py t,
--                               0, 0, 1, pz t,
--                               0, 0, 0, 1 ] (4, 4)
--rotatePoly
----scalePoly

angleBetween :: Point -> Point -> Matrix Double
angleBetween a b = Matrix [ c + (px rotAxis) ** 2 * t, tmp1 - tmp2, tmp3 + tmp4, 0,
                        tmp1 + tmp2, c + (py rotAxis) ** 2 * t, tmp5 + tmp6, 0,
                        tmp3 - tmp4, tmp5 + tmp6, c + (pz rotAxis) ** 2 * t, 0,
                        0, 0, 0, 1 ] (4, 4)
    where c        = cos rotAngle
          s        = sin rotAngle
          t        = 1.0 - c
          tmp1     = (px rotAxis) * (py rotAxis) * t
          tmp2     = (pz rotAxis) * s
          tmp3     = (px rotAxis) * (pz rotAxis) * t
          tmp4     = (py rotAxis) * s
          tmp5     = (py rotAxis) * (pz rotAxis) * t
          tmp6     = (px rotAxis) * s
          rotAxis  = crossProduct a b
          rotAngle = acos $ dotProduct a b

orientPoly :: Point -> Polygon -> Polygon
orientPoly orient p = Polygon (locations p) mtx (upVector p) (rightVector p) (pType p)
    where mtx = dot (transform p) (angleBetween (upVector p) orient)

movePoly :: Point -> Polygon -> Polygon
movePoly pos p = Polygon (locations p) mtx (upVector p) (rightVector p) (pType p)
    where mtx = dot (transform p) (Matrix [ 1, 0, 0, px pos,
                                            0, 1, 0, py pos,
                                            0, 0, 1, pz pos, 
                                            0, 0, 0, 1 ] (4, 4))

scalePoly :: Point -> Polygon -> Polygon
scalePoly scale p = Polygon (locations p) mtx (upVector p) (rightVector p) (pType p)
    where mtx = dot (transform p) (Matrix [ px scale, 0, 0, 0,
                                            0, py scale, 0, 0,
                                            0, 0, pz scale, 0,
                                            0, 0, 0, 1 ] (4, 4))

--data Scene
--    = Polygons [Polygon]
--    | Scenes [Scene]
--    | Transformation (Matrix Double) Scene











