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
import Transform



------------------------------------------------------------------------------
getPoints :: Detail -> [Point]
getPoints (Detail p _ _) = p


------------------------------------------------------------------------------
getShape :: Detail -> Shape
getShape (Detail _ s _) = s


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
	let newM = dotM pMtx
	mapM_ (unwrapTree (l + 1)) sub
	return ()


------------------------------------------------------------------------------
unwrapTree :: Matrix Double -> Int -> Tree Detail -> IO ()
unwrapTree l x = do
    let cLabel = getLabel x
    let cSub =  getSub x

    putStrLn $ replicate (l * 2) ' ' ++ show cLabel

    
    return ()

------------------------------------------------------------------------------
main :: IO ()
main = do
	sphereThenCylinder = do
    	detail Sphere (CubeFaces yAxis) 0.3           ;
    	detail Cylinder (SphereLoop 16 0) 0.3         }
	cubeThenCylinder = do
		detail Cube (CubeFaces xAxis) 0.1             ;
		detail Cylinder (CubeFaces zAxis) 0.9         }
	applyDetail = do {
		detail Cube (CylinderLoop 8 0.5) 0.2          ;
		branch [sphereThenCylinder, cubeThenCylinder] ;
		detail Sphere (CylinderLoop 4 0.2) 0.4        }


    unwrapTree 0 $ head (runDetailGen cubeSegment2)

------------------------------------------------------------------------------
