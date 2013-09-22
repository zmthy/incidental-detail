--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.

module Types where

-- | Standard
import Data.Char

-- | Source
import Hastwix
import Vectwix

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Polygon types
data PolyType = PolyCube | PolySphere | PolyCylinder deriving (Eq, Show, Enum)

--------------------------------------------------------------------------------
-- | Gives a polytype from an integer
pickPolyFromId :: Int -> PolyType
pickPolyFromId id
    | id == 0 = PolySphere
    | id == 1 = PolyCube
    | id == 2 = PolyCylinder
    | otherwise = PolyCube

--------------------------------------------------------------------------------
-- | Gives the Maya command to create the unit mesh of the type
mayaCmdFromPolyType :: PolyType -> String
mayaCmdFromPolyType p
    | p == PolyCube = "cmds.polyCube(w = 2, h = 2, d = 2)" 
    | p == PolySphere = "cmds.polySphere(r = 1)" 
    | p == PolyCylinder = "cmds.polyCylinder(r = 1)" 

--------------------------------------------------------------------------------
-- | Stores a net of locations, a 3D transform, and a few other bits.
data Polygon = Polygon
    { polyType    :: PolyType
    , transform   :: Matrix Double
    }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------