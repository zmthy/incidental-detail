------------------------------------------------------------------------------
module Main
    ( main
    ) where

------------------------------------------------------------------------------
import Data.Matrix

------------------------------------------------------------------------------
import qualified Examples

------------------------------------------------------------------------------
import Graphics.DetailGen.Maya  (writeForest)
import Graphics.DetailGen.Monad (runDetailGen)


------------------------------------------------------------------------------
-- | Writes the simple example to "basic.py".
main :: IO ()
main = writeForest "basic.py" (identity 4) (1, 1, 1) 0 $
    runDetailGen Examples.paperExample2

