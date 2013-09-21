--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.

module Generator where

-- | Standard
import Data.List

-- | Source
import Testing
import Types
import Hastwix
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--|
data Branch = Branch
    { node        :: Polygon
    , parent      :: Branch
    , children    :: [Polygon]
    } | Empty

--------------------------------------------------------------------------------
-- | Prints out a triple.
tDiff :: (Point, Point) -> (Point, Point) -> Bool
tDiff (a, _) (b, _) = dx && dy && dz
    where dx = abs ((px a) - (px b)) < t
          dy = abs ((py a) - (py b)) < t
          dz = abs ((pz a) - (pz b)) < t
          t  = 0.0001
          
--------------------------------------------------------------------------------
-- | Prints out a triple.
unique :: [(Point, Point)] -> [(Point, Point)]
unique xs = nubBy tDiff xs

--------------------------------------------------------------------------------
-- | Prints out a triple.
createBranch :: Matrix Double -> (Point, Point) -> Branch -> Branch
createBranch m loc parent = Branch newNode parent []
    where newNode  = (Polygon locs tForm up right pType)
          locs     = unique $ genSphere 1 4
          tForm    = dot (dot (moveMtx (fst loc)) (dot (orientMtx (snd loc) up) m)) (transform (node parent))
          up       = (0, 1, 0)
          right    = (1, 0, 0)
          pType    = sphere

--------------------------------------------------------------------------------
-- | Prints out a triple.
expand :: Branch -> [Branch]
expand parent = map (\loc -> createBranch (scaleMtx (0.2, 0.6, 0.2)) loc parent) (selectLocations (node parent))

--------------------------------------------------------------------------------
-- | Prints out a triple.
selectLocations :: Polygon -> [(Point, Point)]
selectLocations p = locations p

--------------------------------------------------------------------------------
-- | Prints out a triple.
recur :: Int -> Branch -> [Branch]
recur 0 b = []
recur d b = b : (concat $ map (\br -> recur (d-1) br) (expand b))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------