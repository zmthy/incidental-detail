module Selection
  (
   twopi

    -- * Point
  , Point (..)

    -- * Utils
  , stdUp
  , stdCopy

  , pointOnSphere
  , pointOnCylinder

    -- * Selection Functions
  , bboxCentroids
  , cylinderHLoop
  , sphereHLoop
  , sphereVLoop
  ) where

------------------------------------------------------------------------------
twopi :: Double
twopi = 3.14159265 * 2.0

------------------------------------------------------------------------------
data Point = Point
  { location :: (Double, Double, Double)
  , upVector :: (Double, Double, Double)
  } deriving (Show)


------------------------------------------------------------------------------
stdUp :: Double -> Double -> Double -> Point
stdUp x y z = Point (x, y, z) (0, 1, 0)


------------------------------------------------------------------------------
stdCopy :: Double -> Double -> Double -> Point
stdCopy x y z = Point (x, y, z) (x, y, z)


------------------------------------------------------------------------------
bboxCentroids :: Bool -> Bool-> Bool -> [Point]
bboxCentroids x y z = concat [xA, yA, zA]
  where xA = if x then [stdCopy (-1) 0 0, stdCopy 1 0 0] else []
        yA = if y then [stdCopy 0 (-1) 0, stdCopy 0 1 0] else []
        zA = if z then [stdCopy 0 0 (-1), stdCopy 0 0 1] else []


------------------------------------------------------------------------------
pointOnSphere :: Double -> Double -> Double -> Point
pointOnSphere r p a = stdCopy x y z
  where x = r * sin p * cos a
        y = r * sin p * sin a
        z = r * cos p

------------------------------------------------------------------------------
pointOnCylinder :: Double -> Double -> Double -> Point
pointOnCylinder r a h = Point (x, y, z) (x, 0, z)
  where x = r * cos a
        y = h
        z = r * sin a


------------------------------------------------------------------------------
cylinderHLoop :: Double -> Int -> [Point]
cylinderHLoop h c = map (\a -> pointOnCylinder 1 a h) [0, twopi / (fromIntegral c - 1) .. twopi]


------------------------------------------------------------------------------
sphereHLoop :: Double -> Int -> [Point]
sphereHLoop p c = map (\a -> pointOnSphere 1 p a) [0, twopi / (fromIntegral c - 1) .. twopi]


------------------------------------------------------------------------------
sphereVLoop :: Double -> Int -> [Point]
sphereVLoop h c = sphereHLoop h c



