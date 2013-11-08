------------------------------------------------------------------------------
module Examples
    ( paperExample1
    , paperExample2
    ) where

------------------------------------------------------------------------------
import Graphics.DetailGen.Monad
import Graphics.DetailGen.PointSelection
import Graphics.DetailGen.Vec3



------------------------------------------------------------------------------
uniform :: Double -> Vec3
uniform v = (v, v, v)


------------------------------------------------------------------------------
constant :: Vec3
constant = (1, 1, 1)


------------------------------------------------------------------------------
up :: Vec3
up = (0, 1, 0)


------------------------------------------------------------------------------
fwd :: Vec3
fwd = (1, 0, 0)


------------------------------------------------------------------------------
-- | A simple example of the DSL.
paperExample1 :: DetailGen ()
paperExample1 = applyDetail
  where
    sphereThenCylinder = do
        detail Sphere   (CubeFaces yAxis) (uniform 0.6) constant fwd
        detail Cylinder (SphereLoop 8 0)  (uniform 0.3) constant fwd
    cubeThenCylinder = do
        detail Cube     (CubeFaces xAxis) (uniform 0.4) constant fwd
        detail Cylinder (CubeFaces zAxis) (uniform 0.7) constant fwd
    applyDetail = do
        detail Cylinder Centre constant constant up
        detail Cube     (CylinderLoop 6 0) (uniform 0.2) (1, 2, 1) fwd
        branch [sphereThenCylinder, cubeThenCylinder]
        detail Sphere   (CylinderLoop 4 0) (uniform 0.4) constant fwd


------------------------------------------------------------------------------
-- | A simple example of the DSL.
paperExample2 :: DetailGen ()
paperExample2 = applyDetail
  where
    twoCylindersTo = do
        detail Cylinder (CubeFaces xAxis) (uniform 0.5) constant fwd
        pureBranch [-0.75, 0, 0.75]
    applyDetail = do
        h <- twoCylindersTo
        detail Cube (CylinderLoop 4 h) (uniform 0.2) constant fwd



------------------------------------------------------------------------------
-- | A simple example of the DSL.
{-paperMain :: DetailGen ()-}
{-paperMain = applyDetail-}
  {-where-}
    {-windowPanels = do-}
        {-detail Cube (CylinderLoop 12 (-0.3)) 0.3-}
        {-detail Cylinder (CubeFaces xAxis) 0.5-}
    {-applyDetail = do-}
        {-detail Cylinder Centre 1.0-}
        {-branch [windowPanels]-}

