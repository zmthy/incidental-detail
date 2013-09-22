--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.

module Generator where

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
createBranch :: Location -> Branch -> Branch
createBranch loc parent = Branch newNode parent []
    where newNode  = Polygon PolySphere (dot (transform (node parent)) rts)
          rts      = dot t (dot r s)
          t        = tMtx (fst loc)
          r        = rMtx up (snd loc)
          s        = sMtx (0.3, 0.3, 0.3)
          up       = (0, 1, 0)


--------------------------------------------------------------------------------
-- | Creates a branch at each of the given branchs locations
expand :: Branch -> [Branch]
expand parent = map (\loc -> createBranch loc parent) locs
  where locs = selectLocations (node parent)

--------------------------------------------------------------------------------
-- | Prints out a triple.
recur :: Int -> Branch -> [Branch]
recur 0 b = []
recur d b = b : (concat $ map (\br -> recur (d-1) br) (expand b))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------