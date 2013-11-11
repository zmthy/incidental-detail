------------------------------------------------------------------------------
-- | Defines the 'Point' type and associated functions, for identifying points
-- on a model to generate detail at.
module Graphics.DetailGen.Point
  (
  -- * Type
    Point (..)

  -- * Utilities
  , stdUp
  , stdCopy

  , centroid
  , pointOnSphere
  , pointOnCylinder

  -- * Selection Functions
  , cFront
  , cBack
  , cLeft
  , cRight
  , cTop
  , cBottom
  , bboxCentroids
  , cylinderHLoop
  , sphereLoop
  ) where

------------------------------------------------------------------------------
import Graphics.DetailGen.Vec3


------------------------------------------------------------------------------
data Point = Point
    { location :: Vec3
    , upVector :: Vec3
    } deriving (Show)


------------------------------------------------------------------------------
stdUp :: Double -> Double -> Double -> Point
stdUp x y z = Point (Vec3 x y z) (Vec3 0 1 0)


------------------------------------------------------------------------------
stdCopy :: Double -> Double -> Double -> Point
stdCopy x y z = Point (Vec3 x y z) (Vec3 x y z)


------------------------------------------------------------------------------
centroid :: Point
centroid = stdUp 0 0 0


------------------------------------------------------------------------------
cFront :: Point
cFront = stdCopy 0 0 1


------------------------------------------------------------------------------
cBack :: Point
cBack = stdCopy 0 0 (-1)


------------------------------------------------------------------------------
cLeft :: Point
cLeft = stdCopy (-1) 0 0


------------------------------------------------------------------------------
cRight :: Point
cRight = stdCopy 1 0 0


------------------------------------------------------------------------------
cTop :: Point
cTop = stdCopy 0 1 0


------------------------------------------------------------------------------
cBottom :: Point
cBottom = stdCopy 0 (-1) 0


------------------------------------------------------------------------------
bboxCentroids :: Bool -> Bool-> Bool -> [Point]
bboxCentroids x y z = concat [xA, yA, zA]
  where
    xA = if x then [cRight, cLeft] else []
    yA = if y then [cTop, cBottom] else []
    zA = if z then [cFront, cBack] else []


------------------------------------------------------------------------------
pointOnSphere :: Double -> Double -> Double -> Point
pointOnSphere r p a = stdCopy x y z
  where
    x = r * sin p * cos a
    y = r * sin p * sin a
    z = r * cos p


------------------------------------------------------------------------------
pointOnCylinder :: Double -> Double -> Double -> Point
pointOnCylinder r a h = Point (Vec3 x y z) (Vec3 x 0 z)
  where
    x = r * cos a
    y = h
    z = r * sin a


------------------------------------------------------------------------------
cylinderHLoop :: Double -> Int -> [Point]
cylinderHLoop h c =
    map (flip (pointOnCylinder 1) h) [0, pi * 2 / fromIntegral c .. pi * 2]


------------------------------------------------------------------------------
sphereLoop :: Double -> Int -> [Point]
sphereLoop p c =
    map (pointOnSphere 1 p') [0, pi * 2 / fromIntegral c .. pi * 2]
    where p' = p + pi / 2.0

