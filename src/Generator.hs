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
createBranch :: Int -> Location -> Branch -> Branch
createBranch polyID loc parent = Branch newNode parent []
    where newNode  = Polygon pType (dot (transform (node parent)) trs)
          trs      = dot t (dot r s)
          t        = tMtx (fst loc)
          r        = rMtx up (snd loc)
          s        = sMtx (0.3, 0.7, 0.3)
          up       = (0, 1, 0)
          pType    = pickPolyFromId polyID


--------------------------------------------------------------------------------
-- | Creates a branch at each of the given branchs locations
expand :: Int -> Int -> Branch -> [Branch]
expand selectID polyID parent = map (\loc -> createBranch polyID loc parent) locs
  where locs = selectLocations selectID (node parent)

--------------------------------------------------------------------------------
-- | Prints out a triple.
recur :: Int -> Branch -> [Branch]
recur 0 b = []
recur d b = b : (concat $ map (\br -> recur (d-1) br) (expand (d-1) (d-1) b))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------