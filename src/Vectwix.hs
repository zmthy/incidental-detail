--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.

module Vectwix where

-- Source
import Types
import Hastwix
--------------------------------------------------------------------------------


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
rotMtxBetween a b = add eye (add (mult sinScalar skewed) cosEq
  where eye         = (identity 3)
        cosEq       = mult (1 - (cos angle)) skewSquared
        sinScalar   = sin angle
        cosScalar   = 
        skewSquared = (dot skewed skewed)
        skewed      = skewAxis axis
        angle       = angleBetween  a b
        axis        = axisBetween a b

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