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
    detail Cylinder Centre 1 1 up
    detail Cube     (CylinderLoop 6 0) 0.2 (Vec3 1 2 1) fwd
    branch [sphereThenCylinder, cubeThenCylinder]
    detail Sphere   (CylinderLoop 4 0) 0.4 1 fwd
  where
    sphereThenCylinder = do
        detail Sphere   (CubeFaces yAxis) 0.6 1 fwd
        detail Cylinder (SphereLoop 8 0)  0.3 1 fwd
    cubeThenCylinder = do
        detail Cube     (CubeFaces xAxis) 0.4 1 fwd
        detail Cylinder (CubeFaces zAxis) 0.7 1 fwd


------------------------------------------------------------------------------
-- | Another simple example of the DSL.
paperExample2 :: DetailGen ()
paperExample2 = do
    h <- twoCylindersTo
    detail Cube (CylinderLoop 4 h) 0.2 1 fwd
  where
    twoCylindersTo = do
        detail Cylinder (CubeFaces xAxis) 0.5 1 fwd
        pureBranch [-0.75, 0, 0.75]


------------------------------------------------------------------------------
-- | Main example from the paper (silo)
paperMain1 :: DetailGen ()
paperMain1 = do
    branch [detail Cylinder Centre 1 (Vec3 1 2 1) up]
    branch [beams, walkway, windows, base, ground]
  where
    beams = do
        h <- pureBranch [0.55, 0.85]
        detail Cube (CylinderLoop 12 h) 0.9 (Vec3 0.6 0.04 0.02) fwd
        detail Cylinder (CubeFaces yAxis) 1 (Vec3 0.01 1 0.01) fwd
        detail Sphere (CubeFaces yAxis) 0.02 1 fwd
    walkway = do
        detail Cube (CylinderLoop 12 0.7) 0.9 (Vec3 0.4 0.01 0.4) fwd
        detail Cube FaceR 0.9 (Vec3 0.01 0.4 0.01) fwd
    windows = do
        detail Cube (CylinderLoop 8 0) 0.9 (Vec3 0.1 1 0.2) fwd
        detail Cube FaceR 0.3 (Vec3 0.4 0.5 0.4) fwd
        detail Sphere (CubeFaces zAxis) 0.2 1 fwd
    base = do
        detail Cube (CylinderLoop 12 (-0.7)) 1 (Vec3 0.8 0.02 0.5) fwd
        detail Cube FaceR 0.9 (Vec3 0.02 0.02 0.5) fwd
        detail Cylinder (CubeFaces zAxis) 0.9 (Vec3 0.02 1.2 0.02) fwd
        detail Sphere FaceU 0.05 1 up
    ground = detail Cube FaceD (Vec3 4 0.01 4) 1 up >> done

