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
-- | Gives the Maya name of the poly type.
mayaNameFromPolyType :: PolyType -> String
mayaNameFromPolyType p = [fl] ++ tail str
    where fl  = toLower (str !! 0)
          str = show p

--------------------------------------------------------------------------------
-- | Stores a net of locations, a 3D transform, and a few other bits.
data Polygon = Polygon
    { polyType    :: PolyType
    , transform   :: Matrix Double
    }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------