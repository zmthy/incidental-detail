module Testing where


import Types
import Hastwix

twopi = 3.1415926 * 2

lerp :: Double -> Double -> Double -> Double
lerp a b t = a + (b - a) * t

locOnSphere :: Double -> Double -> Double -> Point
locOnSphere r inc azi = (x, y, z)
    where x = r * (sin inc) * (cos azi)
          y = r * (sin inc) * (sin azi)
          z = r * (cos inc)

genSphere :: Double -> Int -> [Point]
genSphere r d = concat $ map (\i -> loop (twopi / (fromIntegral d) * i)) [0 .. (fromIntegral d) - 1]
    where loop a = map (\i -> locOnSphere r (twopi / (fromIntegral d) * i) a) [0 .. (fromIntegral d) - 1]

genPlane :: Int -> [Point]
genPlane d = concat $ map (\i -> line (lerp 0 1 (1.0 / (fromIntegral d) * i))) [0 .. (fromIntegral d)]
    where line h = map (\i -> (lerp 0 1 (1.0 / (fromIntegral d) * i), h, 0)) [0 .. (fromIntegral d)]


--testPoly = Polygon (genPlane 3) (identity 4) (0, 1, 0) (1, 0, 0)

transformLocs :: [Point] -> Matrix Double -> [Point]
transformLocs locs tForm = map (\p -> toVec (dot tForm (toColMtx p))) locs

updateLocs :: Polygon -> Polygon
updateLocs p = Polygon locs (transform p) (upVector p) (rightVector p) (pType p)
    where locs = transformLocs (locations p) (transform p)

locsAsArray :: [Point] -> [[Double]]
locsAsArray ps = concat $ map (\p -> arrFromPoint p) ps
    where arrFromPoint v = [[px v, py v, pz v]]

--a = twopi / 4
--t2 = Matrix [ 1, 0, 0, 0,
--              0, cos a, -sin a, 0, 
--              0, sin a, cos a, 0, 
--              0, 0, 0, 1 ] (4,4)


--genCube :: Int -> [Point]
--genCube d = concat [p4, p3, p2, p1]
--    where p4 = transformLocs p1 $ angleBetween (0, 1, 0) (0, 0, -1)
--          p3 = transformLocs p1 $ angleBetween (0, 1, 0) (0, (-1), 0)
--          p2 = transformLocs p1 $ angleBetween (0, 1, 0) (0, 0, 1)
--          p1 = genPlane d
