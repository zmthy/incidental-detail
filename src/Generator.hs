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
createBranch :: PolyType -> Location -> Branch -> Branch
createBranch nPolyType loc parent = Branch newNode parent []
    where newNode  = Polygon pType (dot (transform (node parent)) trs)
          trs      = dot t (dot r s)
          t        = tMtx (fst loc)
          r        = rMtx up (snd loc)
          s        = sMtx (0.1, 0.6, 0.1)
          up       = (0, 1, 0)
          pType    = nPolyType

--------------------------------------------------------------------------------
-- | Creates a branch at each of the given branchs locations
expand :: Int -> PolyType -> Branch -> [Branch]
expand selectID nPolyType parent = map (\loc -> createBranch nPolyType loc parent) locs
  where locs = selectLocations selectID (node parent)

--------------------------------------------------------------------------------
-- | Prints out a triple.
recur :: [[(Int, PolyType)]] -> Branch -> [Branch]
recur [] _ = []
recur (i:is) b = b : (concat $ map (\br -> recur is br) branches)
  where branches = concat $ map (\(selId, pType) -> expand selId pType b) i

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------