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
createBranch :: PolyType -> Location -> Point -> Point -> Branch -> Branch
createBranch nPolyType loc gScale pScale parent = Branch newNode parent []
    where newNode  = Polygon pType (dot (transform (node parent)) trs) (sMtx pScale)
          trs      = dot t (dot r s)
          t        = tMtx (fst loc)
          r        = rMtx up (snd loc)
          s        = sMtx gScale
          up       = (0, 1, 0)
          pType    = nPolyType

--------------------------------------------------------------------------------
-- | Creates a branch at each of the given branchs locations
expand :: Int -> PolyType -> Point -> Point -> Branch -> [Branch]
expand selectID nPolyType gS pS parent = map (\loc -> createBranch nPolyType loc gS pS parent) locs
  where locs = transformLocs (selectLocations selectID (node parent)) (pScale (node parent))

--------------------------------------------------------------------------------
-- | Prints out a triple.
recur :: [[(Int, PolyType, Point, Point)]] -> Branch -> [Branch]
recur [] _ = []
recur (i:is) b = b : (concat $ map (\br -> recur is br) branches)
  where branches = concat $ map (\(selId, pType, gScale, pScale) -> expand selId pType gScale pScale b) i

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------