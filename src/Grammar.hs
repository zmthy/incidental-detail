module Grammar where

------------------------------------------------------------------------------
-- | Source
import Types

--------------------------------------------------------------------------------
-- n horizontal rings w n divisions
-- n vertical rings w n divisions 
-- grid over faces
-- randomly remove locations
-- n circle around face centroid (based on normal direction)
-- n faces from mesh
--------------------------------------------------------------------------------

boundingBox :: Polygon -> (Point, Point)
boundingBox polygon = ((lx, ly, lz), (ux, uy, uz))
  where
    (v : vs) = vertices polygon
    search f pf = foldr (f . pf) (pf v) vs
    lx = search min px
    ly = search min py
    lz = search min pz
    ux = search max px
    uy = search max py
    uz = search max pz

locOnSphere :: Double -> Double -> Double -> Point
locOnSphere r inc azi = (x, y, z)
    where x = r * (sin inc)  * (cos azi)
          y = r * (sin inc)  * (sin azi)
          z = r * (cos inc)

inc = acos $ z / r
azi = atan $ y / x

--horiRing :: Polygon -> Double -> Int -> [Point]
--horiRing p t d = 

------------------------------------------------------------------------------
cube :: Polygon
cube = defaultOrientation
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
defaultOrientation :: [Point] -> [(Int, Int, Int)] -> Polygon
defaultOrientation vs fs = Polygon vs fs (0, 1, 0) (1, 0, 0)