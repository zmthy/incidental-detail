------------------------------------------------------------------------------
module Examples
    ( paperExample1
    , paperExample2
    , paperMain1
    ) where

------------------------------------------------------------------------------
import Graphics.DetailGen.Monad
import Graphics.DetailGen.PointSelection
import Graphics.DetailGen.Vec3


------------------------------------------------------------------------------
-- | A simple example of the DSL.
paperExample1 :: DetailGen ()
paperExample1 = do
    detail Cylinder Centre constant constant up
    detail Cube     (CylinderLoop 6 0) (uniform 0.2) (1, 2, 1) fwd
    branch [sphereThenCylinder, cubeThenCylinder]
    detail Sphere   (CylinderLoop 4 0) (uniform 0.4) constant fwd
  where
    sphereThenCylinder = do
        detail Sphere   (CubeFaces yAxis) (uniform 0.6) constant fwd
        detail Cylinder (SphereLoop 8 0)  (uniform 0.3) constant fwd
    cubeThenCylinder = do
        detail Cube     (CubeFaces xAxis) (uniform 0.4) constant fwd
        detail Cylinder (CubeFaces zAxis) (uniform 0.7) constant fwd


------------------------------------------------------------------------------
-- | Another simple example of the DSL.
paperExample2 :: DetailGen ()
paperExample2 = do
    h <- twoCylindersTo
    detail Cube (CylinderLoop 4 h) (uniform 0.2) constant fwd
  where
    twoCylindersTo = do
        detail Cylinder (CubeFaces xAxis) (uniform 0.5) constant fwd
        pureBranch [-0.75, 0, 0.75]


------------------------------------------------------------------------------
-- | Main example from the paper (silo)
paperMain1 :: DetailGen ()
paperMain1 = do
    branch [detail Cylinder Centre constant (1, 2, 1) up]
    branch [beams, walkway, windows, base, ground]
  where
    beams = do
        h <- pureBranch [0.55, 0.85]
        detail Cube (CylinderLoop 12 h) (uniform 0.9) (0.6, 0.04, 0.02) fwd
        detail Cylinder (CubeFaces yAxis) constant (0.01, 1, 0.01) fwd
        detail Sphere (CubeFaces yAxis) (uniform 0.02) constant fwd
    walkway = do
        detail Cube (CylinderLoop 12 0.7) (uniform 0.9) (0.4, 0.01, 0.4) fwd
        detail Cube FaceR (uniform 0.9) (0.01, 0.4, 0.01) fwd
    windows = do
        detail Cube (CylinderLoop 8 0) (uniform 0.9) (0.1, 1, 0.2) fwd
        detail Cube FaceR (uniform 0.3) (0.4, 0.5, 0.4) fwd
        detail Sphere (CubeFaces zAxis) (uniform 0.2) constant fwd
    base = do
        detail Cube (CylinderLoop 12 (-0.7)) constant (0.8, 0.02, 0.5) fwd
        detail Cube FaceR (uniform 0.9) (0.02, 0.02, 0.5) fwd
        detail Cylinder (CubeFaces zAxis) (uniform 0.9) (0.02, 1.2, 0.02) fwd
        detail Sphere FaceU (uniform 0.05) constant up
    ground = detail Cube FaceD (4, 0.01, 4) constant up >> done

