------------------------------------------------------------------------------
-- | Defines 3-dimensional vector utilities.
module Graphics.DetailGen.Vec3
    (
    -- * Vectors
      Vec3 (..)

    -- * Simple constructors
    , up
    , fwd

    -- * Utility functions
    , mtxToArr
    , mtxToArr4
    , multByScalar
    , dotV
    , mag
    , norm
    , angleBetween
    , axisBetween
    , skewAxis
    , rotBetween

    -- * Matrix builders
    , translate
    , rotateX
    , rotateY
    , rotateZ
    , rotate
    , scale

    -- * Operations
    , dotM
    , toVec3
    , dotMV
    ) where

------------------------------------------------------------------------------
import Data.Matrix


------------------------------------------------------------------------------
mtxToArr :: Matrix Double -> [Double]
mtxToArr m =
    [ getElem 1 1 m, getElem 1 2 m, getElem 1 3 m
    , getElem 2 1 m, getElem 2 2 m, getElem 2 3 m
    , getElem 3 1 m, getElem 3 2 m, getElem 3 3 m
    ]


------------------------------------------------------------------------------
mtxToArr4 :: Matrix Double -> [Double]
mtxToArr4 m =
    [ getElem 1 1 m, getElem 1 2 m, getElem 1 3 m, getElem 1 4 m
    , getElem 2 1 m, getElem 2 2 m, getElem 2 3 m, getElem 2 4 m
    , getElem 3 1 m, getElem 3 2 m, getElem 3 3 m, getElem 3 4 m
    , getElem 4 1 m, getElem 4 2 m, getElem 4 3 m, getElem 4 4 m
    ]


------------------------------------------------------------------------------
multByScalar :: Double -> Matrix Double -> Matrix Double
multByScalar s m = fromList 3 3 arr
  where arr = map (*s) (mtxToArr m)


------------------------------------------------------------------------------
data Vec3 = Vec3
    { vx :: Double
    , vy :: Double
    , vz :: Double
    } deriving (Eq, Show)

instance Num Vec3 where
    (+) = vzip (+)
    (-) = vzip (-)
    a * b = Vec3
        (vy a * vz b - vy b * vz a)
        (vx b * vz a - vx a * vz b)
        (vx a * vy b - vy a * vx b)
    abs = vmap abs
    signum = vmap signum
    fromInteger i = let d = fromInteger i in Vec3 d d d

instance Fractional Vec3 where
    (/) = error "Cannot divide vectors"
    recip = error "Cannot divide vectors"
    fromRational r = let d = fromRational r in Vec3 d d d


------------------------------------------------------------------------------
vmap :: (Double -> Double) -> Vec3 -> Vec3
vmap f (Vec3 a b c) = Vec3 (f a) (f b) (f c)


------------------------------------------------------------------------------
vzip :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
vzip f (Vec3 a b c) (Vec3 x y z) = Vec3 (f a x) (f b y) (f c z)


------------------------------------------------------------------------------
-- | The up vector.
up :: Vec3
up = Vec3 0 1 0


------------------------------------------------------------------------------
-- | The forward vector.
fwd :: Vec3
fwd = Vec3 1 0 0


------------------------------------------------------------------------------
dotV :: Vec3 -> Vec3 -> Double
dotV a b = vx a * vx b + vy a * vy b + vz a * vz b


------------------------------------------------------------------------------
mag :: Vec3 -> Double
mag v = sqrt $ vx v ** 2 + vy v ** 2 + vz v ** 2


------------------------------------------------------------------------------
norm :: Vec3 -> Vec3
norm (Vec3 0 0 0) = Vec3 0 0 0
norm v            = vmap (/ mag v) v


------------------------------------------------------------------------------
angleBetween :: Vec3 -> Vec3 -> Double
angleBetween vA vB = acos $ dotV (norm vA) (norm vB)


------------------------------------------------------------------------------
axisBetween :: Vec3 -> Vec3 -> Vec3
axisBetween vA vB
    | vA * vB == Vec3 0 0 0 = Vec3 0 0 1
    | otherwise = norm $ vA * vB


------------------------------------------------------------------------------
skewAxis :: Vec3 -> Matrix Double
skewAxis a = fromList 3 3 [ 0,     -vz a, vy a,
                            vz a,  0,     -vx a,
                            -vy a, vx a,  0     ]


------------------------------------------------------------------------------
addHomo :: Matrix Double -> Matrix Double
addHomo m = fromList 4 4
    [ getElem 1 1 m, getElem 1 2 m, getElem 1 3 m, 0
    , getElem 2 1 m, getElem 2 2 m, getElem 2 3 m, 0
    , getElem 3 1 m, getElem 3 2 m, getElem 3 3 m, 0
    , 0,             0,             0,             1
    ]


------------------------------------------------------------------------------
rotBetween :: Vec3 -> Vec3 -> Matrix Double
rotBetween a b = addHomo $ eye + (sinM + cosM)
  where eye    = identity 3
        sinM   = multByScalar (sin angle) skewed
        cosM   = multByScalar (1 - cos angle) (skewed * skewed)
        skewed = skewAxis (axisBetween a b)
        angle  = angleBetween a b


------------------------------------------------------------------------------
translate :: Vec3 -> Matrix Double
translate (Vec3 x y z) = fromList 4 4
    [ 1, 0, 0, x
    , 0, 1, 0, y
    , 0, 0, 1, z
    , 0, 0, 0, 1
    ]


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
rotate _ _ = identity 4


------------------------------------------------------------------------------
scale :: Vec3 -> Matrix Double
scale (Vec3 x y z) = fromList 4 4
    [ x, 0, 0, 0
    , 0, y, 0, 0
    , 0, 0, z, 0
    , 0, 0, 0, 1
    ]


------------------------------------------------------------------------------
dotM :: Matrix Double -> Matrix Double -> Matrix Double
dotM = multStd


------------------------------------------------------------------------------
toVec3 :: Matrix Double -> Vec3
toVec3 m = Vec3 (getElem 1 1 m) (getElem 2 1 m) (getElem 3 1 m)


------------------------------------------------------------------------------
dotMV :: Matrix Double -> Vec3 -> Vec3
dotMV m v = toVec3 $ dotM m v'
  where v' = fromList 4 1 [vx v, vy v, vz v, 1.0]


