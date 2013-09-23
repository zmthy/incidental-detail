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

type InstructSet = (Int, PolyType, Point, Point, Point, [Int], [Double])

data BranchStump = BranchStump
    { 
      expansion :: InstructSet,
      childs :: [BranchStump]
    }

data Branch = Branch
    { node        :: Polygon
    , parent      :: Branch
    , children    :: [Polygon]
    } | Empty

--------------------------------------------------------------------------------
-- | Prints out a triple.
createBranch :: PolyType -> Location -> Point -> Point -> Point-> Branch -> Branch
createBranch nPolyType loc gScale pScale up parent = Branch newNode parent []
    where newNode  = Polygon pType (dot (transform (node parent)) trs) (sMtx pScale)
          trs      = dot t (dot r s)
          t        = tMtx (fst loc)
          r        = rMtx up (snd loc)
          s        = sMtx gScale
          pType    = nPolyType

--------------------------------------------------------------------------------
-- | Creates a branch at each of the given branchs locations
expand :: InstructSet -> Branch -> [Branch]
expand (selectID, nPolyType, gS, pS, up, ints, doubs) parent = map (\loc -> createBranch nPolyType loc gS pS up parent) locs
  where locs = transformLocs (selectLocations selectID (node parent) ints doubs) (pScale (node parent))

--------------------------------------------------------------------------------
-- | Prints out a triple.
--recur :: BranchStump -> Branch -> [Branch]
--recur [] _ = []
--recur (i:is) b = b : (concat $ map (\br -> recur is br) branches)
--  where branches = concat $ map (\(selId, pType, gScale, pScale, up, ints, doubs) -> expand selId pType gScale pScale up ints doubs b) i

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--recur :: [BranchStump] -> Branch -> [Branch]
--recur (BranchStump _ []) b = [b]
--recur bStump bParent = bParent : (concat $ map (\(currentChild, createdBranch) -> recur currentChild createdBranch) combs)
--  where combs = [(c, b) | c <- (childs bStump), b <- createdBranches]
--        createdBranches = expand (expansion bStump) bParent

recur :: [BranchStump] -> Branch -> [Branch]
recur [] b = [b]
recur bStumps bParent = bParent : (concat $ map (\bs -> results bs) bStumps)
  where results bStump = concat $ map (\aBranch -> recur (childs bStump) aBranch) (expand (expansion bStump) bParent)


--spawn children
--each child recur