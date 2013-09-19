------------------------------------------------------------------------------
module Main where

------------------------------------------------------------------------------
import Data.Matrix


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
data Polygon = Polygon
    { vertices    :: [Point]
    , faces       :: [(Int, Int, Int)]
    , upVector    :: Point
    , rightVector :: Point
    }


data Scene
    = Polygons [Polygon]
    | Scenes [Scene]
    | Transformation (Matrix Double) Scene


------------------------------------------------------------------------------
-- | Rotates the scene in the z-axis by the given radians.
rotate :: Double -> Scene -> Scene
rotate r (Transformation m s) = Transformation (m * rm) s
  where
    rm = fromLists
        [ [cos r, -sin r, 0, 0]
        , [sin r, cos r, 0, 0]
        , [0, 0, 1, 0]
        , [0, 0, 0, 1]
        ]
rotate r ps = rotate r $ Transformation (identity 4) ps


------------------------------------------------------------------------------
-- | Translates the scene with the given vector.
translate :: Point -> Scene -> Scene
translate (x, y, z) (Transformation m s) = Transformation (m * tm) s
  where
    tm = fromLists
        [ [1, 0, 0, x]
        , [0, 1, 0, y]
        , [0, 0, 1, z]
        , [0, 0, 0, 1]
        ]
translate t ps = translate t $ Transformation (identity 4) ps


------------------------------------------------------------------------------
data Location = Location
    { vertex      :: Point
    , orientation :: Point
    }


------------------------------------------------------------------------------
defaultOrientation :: [Point] -> [(Int, Int, Int)] -> Polygon
defaultOrientation vs fs = Polygon vs fs (0, 1, 0) (1, 0, 0)



------------------------------------------------------------------------------
cuboid :: Polygon
cuboid = defaultOrientation
    [ (1, 1, 1)
    , (1, 1, -1)
    , (1, -1, 1)
    , (1, -1, -1)
    , (-1, 1, 1)
    , (-1, 1, -1)
    , (-1, -1, 1)
    , (-1, -1, -1)
    ]
    [ (0, 1, 3)
    , (0, 3, 2)
    , (7, 3, 2)
    , (6, 7, 2)
    , (7, 5, 1)
    , (7, 5, 3)
    , (4, 5, 7)
    , (6, 4, 7)
    , (4, 0, 2)
    , (6, 4, 2)
    , (5, 1, 0)
    , (4, 5, 0)
    ]


------------------------------------------------------------------------------
circle :: Int -> Double -> Scene -> Scene
circle n r s = Scenes $
    map (\i -> rotate (rf i) $ translate (r, 0, 0) s) [0..n-1]
  where rf i = fromIntegral i * 2 * pi / fromIntegral n


------------------------------------------------------------------------------
{-boundingBox :: Shape -> (Point, Point)-}
{-boundingBox shape = ((lx, ly, lz), (ux, uy, uz))-}
  {-where-}
    {-(v : vs) = vertices shape-}
    {-search f pf = foldr (f . pf) (pf v) vs-}
    {-lx = search min px-}
    {-ly = search min py-}
    {-lz = search min pz-}
    {-ux = search max px-}
    {-uy = search max py-}
    {-uz = search max pz-}


------------------------------------------------------------------------------
{-genLocFromLoop :: Double -> Shape -> Shape-}
{-genLocFromLoop t shape = undefined-}
  {-where-}
    {-(l, u) = boundingBox shape-}
    {-ly = py l-}
    {-uy = py u-}
    {-p = ly + (uy - ly) * t-}


-- location, a vector where detail can be added
-- shape, a vertex mesh with an up vector and a right vector
-- location set, a list o a locations.

-- centroid, fn to get the centroid of a given shape
-- mutateShape, fn to rebuild the location set


-- 0InRing
-- subdivdeBetween


------------------------------------------------------------------------------
main :: IO ()
main = return ()

