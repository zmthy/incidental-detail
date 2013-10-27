--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.
-- |
-- | Victoria University of Welling, ECS
-- |    Richard Roberts
-- |    Timothy Jones
-- |    John Lewis

module Main where

-- | Standard
import Text.Printf

-- | Source
import Maya
import Generator
import Types
import Hastwix
import Vectwix
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Prints out a triple.
showTriple :: Point -> IO ()
showTriple p = putStrLn $ printf "x:%2.2f, y:%2.2f, z:%2.2f" (px p) (py p) (pz p)

--------------------------------------------------------------------------------
-- | Writes a python script based on the generated pattern.
main :: IO ()
main = do
    -- Make the first branch of the pattern.
    let org = Polygon PolyCylinder (tMtx (0, 0, 0)) (sMtx (1, 1, 1))
    let b1 = Branch org Empty []

    -- Give the recursion instructions
    --let fake = BranchStump (0, PolyCube, (0.3, 0.3, 0.3), (0.4, 0.05, 1.2), (0, 0, 1), [1,1,1], [0.5]) []

    let cS2 = BranchStump (0, PolyCube, (0.3, 0.3, 0.3), (1,1,1), (0, 1, 0), [1, 1, 1], []) []
    let cS1 = BranchStump (1, PolyCube, (0.3, 0.3, 0.3), (1,1,1), (1, 0, 0), [8], [0.99]) []

    let bS1 = BranchStump (0, PolyCylinder, (0.3, 0.3, 0.3), (1,1,1), (0, 1, 0), [0, 1, 0], []) [cS2, cS1]
    let bS2 = BranchStump (0, PolyCylinder, (0.3, 0.3, 0.3), (1,1,1), (0, 1, 0), [0, 0, 1], []) [cS2]

    -- Recusrively expand it.
    let pSet = map (\b -> (node b)) (recur [bS1, bS2] b1)

    -- Write the python script
    let title = "basic"
    let cmds = polySetToCmds pSet
    writeFile (title ++ ".py") $ (preamble title ++ cmds)

    return ()

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------