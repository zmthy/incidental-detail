--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.
module LocationSelection where

-- | Standard
import Data.List
import Control.Arrow
import Text.Printf

-- | Source
import Types
import Hastwix
import Vectwix
--------------------------------------------------------------------------------

type Location = (Point, Point)

--------------------------------------------------------------------------------
-- | Prints out a triple.
tDiff :: Location -> Location -> Bool
tDiff (a, _) (b, _) = dx && dy && dz
    where dx = abs ((px a) - (px b)) < t
          dy = abs ((py a) - (py b)) < t
          dz = abs ((pz a) - (pz b)) < t
          t  = 0.0001
          
--------------------------------------------------------------------------------
-- | Prints out a triple. 
unique :: [Location] -> [Location]
unique xs = nubBy tDiff xs

--------------------------------------------------------------------------------
-- | two pie are.
twopi = 3.1415926 * 2

--------------------------------------------------------------------------------
-- | Transforms each point in an array of locations with the given matrix.
transformLocs :: [Location] -> Matrix Double -> [Location]
transformLocs locs tForm = map (\(p, o) -> (asVec (dot tForm (asCol p)), asVec (dot tForm (asCol o)))) locs


--------------------------------------------------------------------------------
-- | Linear interpolation
lerp :: Double -> Double -> Double -> Double
lerp a b t = a + (b - a) * t

--------------------------------------------------------------------------------
-- | Gives a point on a the surface of a sphere give the incident and 
-- | azimuth angles.
locOnSphere :: Double -> Double -> Double -> Location
locOnSphere r polar azi = ((x, y, z), (x, y, z))
    where x = r * (sin polar) * (cos azi)
          y = r * (sin polar) * (sin azi)
          z = r * (cos polar)

locOnCylinder :: Double -> Double -> Double -> Location
locOnCylinder p azi h = ((x, y, z), (x, 0, z))
  where x = p * (cos azi)
        y = h
        z = p * (sin azi)

--------------------------------------------------------------------------------
-- | Generates a sphere represented by an array of locations

opposites :: Int -> Int -> Int -> [Location]
opposites x y z = (if not(x == 0) then [right,   left]   else []) ++
               (if not(y == 0) then [top,     bottom] else []) ++
               (if not(z == 0) then [forward, back]   else [])
    where right   = (( s,  0,  0), ( s,  0,  0))
          left    = ((-s,  0,  0), (-s,  0,  0))
          top     = (( 0,  s,  0), ( 0,  s,  0))
          bottom  = (( 0, -s,  0), ( 0, -s,  0))
          forward = (( 0,  0,  s), ( 0,  0,  s))
          back    = (( 0,  0, -s), ( 0,  0, -s))
          s       = 1.0

vertLoop :: Double -> Int -> [Location]
vertLoop polar incs = map (\a -> fn a) [0 .. (fromIntegral incs) - 1]
  where fn a = locOnSphere 1 polar (twopi / (fromIntegral incs) * a)

horiLoop :: Double -> Int -> [Location]
horiLoop polar incs = transformLocs locs (rMtx (0, 0, 1) (0, 1, 0))
  where locs = vertLoop polar incs

cHoriLoop :: Double -> Int -> [Location]
cHoriLoop z incs = map (\a -> fn a) [0 .. (fromIntegral incs) - 1]
  where fn a = locOnCylinder 1 (twopi / (fromIntegral incs) * a) z

allSphere :: Double -> Int -> [Location]
allSphere r d = concat $ map (\i -> loop (twopi / (fromIntegral d) * i)) [0 .. (fromIntegral d) - 1]
    where loop a = map (\i -> locOnSphere r (twopi / (fromIntegral d) * i) a) [0 .. (fromIntegral d) - 1]

showTriple :: Point -> String
showTriple p = printf "x:%2.2f, y:%2.2f, z:%2.2f" (px p) (py p) (pz p)

stringsFromLoc :: [Location] -> [String]
stringsFromLoc ls = map (\(a, b) -> (showTriple a) ++ " || " ++ (showTriple b)) ls

safenDouble :: (Fractional a, Integral b, RealFrac a1) => b -> a1 -> a
safenDouble n d = (fromInteger $ round $ d * (10 ^ n)) / (10.0 ^^ n)

safenLoc :: Location -> Location
safenLoc (pos, ori) = ((xA, yA, zA), (xB, yB, zB))
    where xA = makeSafe $ px pos
          yA = makeSafe $ py pos
          zA = makeSafe $ pz pos
          xB = makeSafe $ px ori
          yB = makeSafe $ py ori
          zB = makeSafe $ pz ori
          makeSafe = safenDouble 4

safenLocs :: [Location] -> [Location]
safenLocs locs = map safenLoc locs

--------------------------------------------------------------------------------
-- | Selects all locations
pickSelFromId :: PolyType -> Int -> [Int] -> [Double] -> [Location]
pickSelFromId pType selectId ints dubs
    | (pType == PolyCube     && selectId == 0) = safenLocs $ opposites (ints !! 0) (ints !! 1) (ints !! 2) -- cube
    | (pType == PolySphere   && selectId == 0) = safenLocs $ opposites (ints !! 0) (ints !! 1) (ints !! 2) -- sphere
    | (pType == PolySphere   && selectId == 1) = safenLocs $ vertLoop (dubs !! 0) (ints !! 0) -- sphere
    | (pType == PolySphere   && selectId == 2) = safenLocs $ horiLoop (dubs !! 0) (ints !! 0) -- sphere
    | (pType == PolyCylinder && selectId == 0) = safenLocs $ opposites (ints !! 0) (ints !! 1) (ints !! 2)     -- cylinder
    | (pType == PolyCylinder && selectId == 1) = safenLocs $ cHoriLoop (dubs !! 0) (ints !! 0) -- cylinder
    | otherwise = []

--------------------------------------------------------------------------------
-- | Selects all locations
selectLocations :: Int -> Polygon -> [Int] -> [Double] -> [Location]
selectLocations selectID p ints dubs = unique (pickSelFromId (polyType p) selectID ints dubs)
