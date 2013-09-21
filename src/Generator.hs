module Generator where

--------------------------------------------------------------------------------
-- | 
import Testing
import Types
import Hastwix

import Data.List

--generate (poly)
--    fn = randomLocationSelectionFn()
--    selectedLocations = fn(poly)

--    polyType = randomPolyType()
--    each loc in selectedLocations()
--        newPoly = genPoly(polyType)
--        newPoly.setTransform(poly.transform, loc.transform)
--        generate(newPoly)


--generate :: [Polygon] -> Polygon -> [Polygon]
--generate ps p = 

data Branch = Branch
    { node        :: Polygon
    , parent      :: Branch
    , children    :: [Polygon]
    } | Empty

--generate :: [Branch] -> [Branch]
--generate b:bs = 

--generate :: [Branch] -> [Branch]
--generate all@(b:bs) = map (\somelamba) (locations b)
--


--get locations from given branch's node.
--setChildren
--for each of those locations, add a new branch
--    set parent to current branch
--    move to given location
--    generateNewLocations
--    branch(locations)

tDiff :: (Point, Point) -> (Point, Point) -> Bool
tDiff (a, _) (b, _) = dx && dy && dz
    where dx = abs ((px a) - (px b)) < t
          dy = abs ((py a) - (py b)) < t
          dz = abs ((pz a) - (pz b)) < t
          t  = 0.0001

unique :: [(Point, Point)] -> [(Point, Point)]
unique xs = nubBy tDiff xs

--createBranch loc parent = Branch node parent (expand node)
createBranch :: Matrix Double -> (Point, Point) -> Branch -> Branch
createBranch m loc parent = Branch newNode parent []
    where newNode  = (Polygon locs tForm up right pType)
          locs     = unique $ genSphere 1 4
          --tForm    = dot (moveMtx (fst loc)) (dot (orientMtx (snd loc) up) (dot m (identity 4)))
          tForm    = dot (dot (moveMtx (fst loc)) (dot (orientMtx (snd loc) up) m)) (transform (node parent))
          up       = (0, 1, 0)
          right    = (1, 0, 0)
          pType    = sphere

expand :: Branch -> [Branch]
expand parent = map (\loc -> createBranch (scaleMtx (0.2, 0.6, 0.2)) loc parent) (selectLocations (node parent))


selectLocations :: Polygon -> [(Point, Point)]
selectLocations p = locations p


recur :: Int -> Branch -> [Branch]
recur 0 b = []
recur d b = b : (concat $ map (\br -> recur (d-1) br) (expand b))