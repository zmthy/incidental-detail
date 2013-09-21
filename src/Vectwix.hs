--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.

module Vectwix where

-- Source
import Hastwix
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Point, and a collection of utility functions.
type Point = (Double, Double, Double)

--------------------------------------------------------------------------------
-- | X component of a point
px :: Point -> Double
px (x, _, _) = x

--------------------------------------------------------------------------------
-- | Y component of a point
py :: Point -> Double
py (_, y, _) = y

--------------------------------------------------------------------------------
-- | Z component of a point
pz :: Point -> Double
pz (_, _, z) = z

--------------------------------------------------------------------------------
-- | Cross product of two points.
cross :: Point -> Point -> Point
cross a b = (x, y, z)
  where x = (py a) * (pz b) - (py b) * (pz a)
        y = (px b) * (pz a) - (px a) * (pz b)
        z = (px a) * (py b) - (py a) * (px b)

--------------------------------------------------------------------------------
-- | Dot product of two points.
dotP :: Point -> Point -> Double
dotP a b = (px a) * (px b) + (py a) * (py b) + (pz a) * (pz b)

--------------------------------------------------------------------------------
-- | Magnitude of a point (or technically vector)
mag :: Point -> Double
mag v = sqrt $ px v ** 2 + py v ** 2 + pz v ** 2

--------------------------------------------------------------------------------
-- | Normalizes a point (or technically vector)
norm :: Point -> Point
norm v = (x, y, z)
  where x = (px v) / m
        y = (py v) / m
        z = (pz v) / m
        m = mag v

--------------------------------------------------------------------------------
-- | Converts a point to column matrix
asCol :: Point -> Matrix Double
asCol p = Matrix [px p, py p, pz p, 1] (4, 1)

--------------------------------------------------------------------------------
-- | Converts a column matrix to a point
asVec :: Matrix Double -> Point
asVec (Matrix [x, y, z, _] shape) = (x, y, z)

--------------------------------------------------------------------------------
-- | Gives the angle between two vectors (inverse cosine of their dot product)
angleBetween :: Point -> Point -> Double 
angleBetween vA vB = acos $ dotP (norm vA) (norm vB)

--------------------------------------------------------------------------------
-- | Normalized cross product between two vectors (axis of rotation)
axisBetween :: Point -> Point -> Point
axisBetween vA vB
    | (cross vA vB) == (0, 0, 0) = (1, 0, 0)
    | otherwise = norm $ cross vA vB

--------------------------------------------------------------------------------
-- | Creates a skew-symmetric cross-product matrix from the given axis.
skewAxis :: Point -> Matrix Double
skewAxis a = Matrix [     0,  pz a, -py a,
                      -pz a,     0,  px a,
                       py a, -px a,     0 ] (3, 3)

--------------------------------------------------------------------------------
-- | Adds homogenous co-ordinates to a 3x3 rotation matrix
addHomo :: Matrix Double -> Matrix Double
addHomo (Matrix m dim) = Matrix [ m !! 0, m !! 1, m !! 2, 0, 
                                  m !! 3, m !! 4, m !! 5, 0,
                                  m !! 6, m !! 7, m !! 8, 0, 
                                  0, 0, 0, 1 ] (4, 4)

--------------------------------------------------------------------------------
-- | Generates a rotation matrix who will transform the given point a to b.
rotMtxBetween :: Point -> Point -> Matrix Double
rotMtxBetween a b = add eye (add sinEq cosEq)
  where eye         = (identity 3)
        sinEq       = mult (sin angle) skewed
        cosEq       = mult (1 - (cos angle)) (dot skewed skewed)
        skewed      = skewAxis (axisBetween a b)
        angle       = angleBetween  a b

--------------------------------------------------------------------------------
-- | Makes a translation matrix given a point
tMtx :: Point -> Matrix Double
tMtx pos = Matrix [ 1, 0, 0, px pos,
                    0, 1, 0, py pos,
                    0, 0, 1, pz pos, 
                    0, 0, 0, 1 ] (4, 4)

--------------------------------------------------------------------------------
-- | Makes a rotation matrix given a point
rMtx :: Point -> Point -> Matrix Double
rMtx a b = addHomo $ rotMtxBetween a b


--------------------------------------------------------------------------------
-- | Makes a scale matrix given a point
sMtx :: Point -> Matrix Double
sMtx scale = Matrix [ px scale, 0, 0, 0,
                      0, py scale, 0, 0,
                      0, 0, pz scale, 0,
                      0, 0, 0, 1 ] (4, 4)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------