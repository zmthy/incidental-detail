------------------------------------------------------------------------------
module Transform
  (
  -- * Builders
    translate
  , rotateX
  , rotateY
  , rotateZ
  , rotate
  , scale

  -- * Operations
  , dotM
  ) where


import Data.Matrix



------------------------------------------------------------------------------
translate :: Double -> Double -> Double -> Matrix Double
translate x y z = fromList 4 4 arr
  where arr = [ 1, 0, 0, x,
                0, 1, 0, y,
                0, 0, 1, z,
                0, 0, 0, 1 ]


------------------------------------------------------------------------------
rotateX :: Double -> Matrix Double
rotateX v = fromList 4 4 arr
  where arr = [ 1,      0,     0,      0,
                0,      cos v, -sin v, 0,
                0,      sin v, cos v,  0,
                0,      0,     0,      1 ]


------------------------------------------------------------------------------
rotateY :: Double -> Matrix Double
rotateY v = fromList 4 4 arr
  where arr = [ cos v,  0, sin v,      0,
                0,      1,     0,      0,
                -sin v, 0, cos v,      0,
                0,      0,     0,      1 ]


------------------------------------------------------------------------------
rotateZ :: Double -> Matrix Double
rotateZ v = fromList 4 4 arr
  where arr = [ cos v,  -sin v, 0,     0,
                sin v,  cos v,  0,     0,
                0,      0,      1,     0,
                0,      0,      0,     1 ]


------------------------------------------------------------------------------
rotate :: Int -> Double -> Matrix Double
rotate 0 v = rotateX v
rotate 1 v = rotateY v
rotate 2 v = rotateZ v


------------------------------------------------------------------------------
scale :: Double -> Double -> Double -> Matrix Double
scale x y z = fromList 4 4 arr
  where arr = [ x, 0, 0, 0,
                0, y, 0, 0,
                0, 0, z, 0,
                0, 0, 0, 1 ]


------------------------------------------------------------------------------
dotM :: Matrix Double -> Matrix Double -> Matrix Double

dotM = multStd


