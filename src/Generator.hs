--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.

module Generator where

-- | Standard
import Data.List

-- | Source
import Types
import Hastwix
import Vectwix
import LocationSelection
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- |
data Branch = Branch
    { node        :: Polygon
    , parent      :: Branch
    , children    :: [Polygon]
    } | Empty

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
-- | Prints out a triple.
createBranch :: Matrix Double -> Location -> Branch -> Branch
createBranch m loc parent = Branch newNode parent []
    where newNode  = updateLocs $ (Polygon locs tForm up right pType)
          locs     = unique $ genSphere 1 2
          tForm    = dot (dot (tMtx (fst loc)) (dot (rMtx (fst loc) up) m)) (transform (node parent))
          up       = asVec $ dot (transform (node parent)) (asCol (0, 1, 0))
          right    = asVec $ dot (transform (node parent)) (asCol (1, 0, 0))
          pType    = sphere

--------------------------------------------------------------------------------
-- | Prints out a triple.
expand :: Branch -> [Branch]
expand parent = map (\loc -> createBranch (sMtx (0.2, 0.2, 0.2)) loc parent) (selectLocations (node parent))
--expand parent = map (\loc -> createBranch (sMtx (0.2, 0.6, 0.2)) loc parent) (horiRing 0.5 (node parent))

--------------------------------------------------------------------------------
-- | Prints out a triple.
recur :: Int -> Branch -> [Branch]
recur 0 b = []
recur d b = b : (concat $ map (\br -> recur (d-1) br) (expand b))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------