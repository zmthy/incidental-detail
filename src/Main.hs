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
import Data.Matrix

-- Source
import DetailGen
import PointSelect
import Selection
import Transform


------------------------------------------------------------------------------
getShape :: Detail -> Shape
getShape (Detail s _ _) = s


------------------------------------------------------------------------------
getPoints :: Detail -> [Point]
getPoints (Detail _ p _) = toPoints p


------------------------------------------------------------------------------
getScale :: Detail -> Double
getScale (Detail _ _ s) = s

------------------------------------------------------------------------------
getLabel :: Tree Detail -> Detail
getLabel = rootLabel


------------------------------------------------------------------------------
getSub :: Tree Detail -> Forest Detail
getSub = subForest


------------------------------------------------------------------------------
expand :: Point -> Matrix Double -> Int -> Forest Detail -> IO ()
expand p pMtx l sub = do
	let newM = dotM pMtx (identity 4)
	mapM_ (unwrapTree newM (l + 1)) sub
	return ()


------------------------------------------------------------------------------
unwrapTree :: Matrix Double -> Int -> Tree Detail -> IO ()
unwrapTree m l x = do
    let cLabel = getLabel x
    let cSub =  getSub x

    putStrLn $ replicate (l * 2) ' ' ++ show (getShape cLabel)

    return ()

------------------------------------------------------------------------------
main :: IO ()
main = do
	let sphereThenCylinder = do {
    	detail Cylinder (CubeFaces yAxis) 0.3 ;
    	detail Cylinder (CubeFaces xAxis) 0.3 }
	let cubeThenCylinder = do {
		detail Cube (CubeFaces xAxis) 0.1 ;
		detail Cube (CubeFaces zAxis) 0.9 }
	let applyDetail = do {
		detail Cube (CylinderLoop 8 0.5) 0.2 ;
		branch [sphereThenCylinder, cubeThenCylinder] ;
		detail Cube (CylinderLoop 4 0.2) 0.4 }

    unwrapTree (identity 4) 0 $ head (runDetailGen applyDetail)

------------------------------------------------------------------------------
