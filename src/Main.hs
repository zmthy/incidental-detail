------------------------------------------------------------------------------
-- Incidental Detail, a library for generating details of the incidental kind.
--
-- Victoria University of Welling, ECS
--    Richard Roberts
--    Timothy Jones
--    John Lewis
------------------------------------------------------------------------------

module Main where

-- Standard
import Control.Applicative
import Data.Tree

-- Source
import DetailGen
import PointSelect

-- Get the current label.
getLabel :: Tree Detail -> Detail
getLabel = rootLabel

getSub :: Tree Detail -> Forest Detail
getSub = subForest

unwrapTree :: Int -> Tree Detail -> IO ()
unwrapTree l x = do
    let cLabel = getLabel x
    let cSub =  getSub x

    putStrLn $ replicate (l * 2) ' ' ++ show cLabel

    mapM_ (unwrapTree (l+1)) cSub
    return ()

------------------------------------------------------------------------------
main :: IO ()
main = do
    let segment = do { detail Cylinder (CubeFaces xAxis) 0.5 ; branch [pure 0.25, pure 0.5, pure 0.7]}
    let cubeSegment = do { height <- segment ; detail Cube (CylinderLoop 4 height) 0.2 ; branch [pure 0.2]}
    let cubeSegment2 = do { height2 <- cubeSegment ; detail Cube (CylinderLoop 4 height2) 0.2 }
    unwrapTree 0 $ head (runDetailGen cubeSegment2)

------------------------------------------------------------------------------
