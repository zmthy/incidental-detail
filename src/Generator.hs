module Generator where

--------------------------------------------------------------------------------
-- | 
import Testing
import Types
import Hastwix

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

--createBranch loc parent = Branch node parent (expand node)
createBranch :: (Polygon -> Polygon) -> Point -> Branch -> Branch
createBranch fn loc parent = Branch newNode parent []
    where newNode  = fn $ movePoly loc (Polygon locs tForm up right pType)
          locs     = genSphere 1 4
          tForm    = dot (identity 4) (transform (node parent))
          up       = (0, 1, 0)
          right    = (1, 0, 0)
          pType    = sphere

expand :: Branch -> [Branch]
expand parent = map (\loc -> createBranch (scalePoly (0.25, 0.6, 0.25)) loc parent) (selectLocations (node parent))


selectLocations :: Polygon -> [Point]
selectLocations p = take 8 (locations p)


recur :: Int -> Branch -> [Branch]
recur 0 b = []
recur d b = b : (concat $ map (\br -> recur (d-1) br) (expand b))