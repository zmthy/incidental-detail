------------------------------------------------------------------------------
module Main
    ( main
    ) where

------------------------------------------------------------------------------
import Data.Matrix
import Data.Tree
import Text.Printf

------------------------------------------------------------------------------
import Graphics.DetailGen.Monad
import Graphics.DetailGen.Point
import Graphics.DetailGen.PointSelection
import Graphics.DetailGen.Vec3

import Examples


------------------------------------------------------------------------------
-- | Writes the simple example to "basic.py".
main :: IO ()
main = do
    writeFile "basic.py" (preamble "Basic")
    unwrapTree (identity 4) (1, 1, 1) 0 $ head (runDetailGen paperExample1)

------------------------------------------------------------------------------
makeDelims :: String -> String
makeDelims d = "# " ++  concat (replicate 76 d) ++ " #"


------------------------------------------------------------------------------
preamble :: String -> String
preamble n = unlines [delims, comment, delims, "", importMC, "", def]
    where delims   = makeDelims "-"
          comment  = printf "# %s, generated by IDG." n
          importMC = "import maya.cmds as cmds"
          def      = "def generate():"


------------------------------------------------------------------------------
cmdFromShape :: Shape -> String
cmdFromShape Cube     = "cmds.polyCube(w=2, h=2, d=2)"
cmdFromShape Cylinder = "cmds.polyCylinder(r=1, h=2)"
cmdFromShape Sphere   = "cmds.polySphere(r=1)"

------------------------------------------------------------------------------
formMatrix :: Point -> Vec3 -> Vec3 -> Matrix Double
formMatrix p gScale upV = t * r * s
    where t = translate loc
          r = rotBetween upV up
          s = scale gScale
          loc = location p
          up  = upVector p


------------------------------------------------------------------------------
expand :: Int -> Detail -> Forest Detail -> Matrix Double -> Vec3 -> Vec3 -> Vec3 -> Point -> IO ()
expand l label sub pMtx pScale gScale up p = do
    --print $ show p
    let newM = pMtx * formMatrix p gScale up
        newMScale = scale pScale * newM
    appendFile "basic.py" $ printf "    %s\n" (cmdFromShape (detailShape label))
    appendFile "basic.py" $ printf "    cmds.xform(m = %s)\n" $ show (mtxToArr4 (transpose newMScale))
    mapM_ (unwrapTree newM pScale (l + 1)) sub


------------------------------------------------------------------------------
unwrapTree :: Matrix Double -> Vec3 -> Int -> Tree Detail -> IO ()
unwrapTree m s l x = do
    let root     = rootLabel x
        children = subForest x
        points   = toPoints $ detailSelection root
        points'  = map (\(Point a b) -> (Point (dotMV (scale s) a) b)) points
        gScale   = detailGScale root
        pScale   = detailPScale root
        up       = detailUp root
    mapM_ (expand l root children m pScale gScale up) points'

