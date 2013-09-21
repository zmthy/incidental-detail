------------------------------------------------------------------------------
module Main where

------------------------------------------------------------------------------
-- | Source
import Maya

import Generator
import Testing
import Types
import Hastwix

------------------------------------------------------------------------------
---- |
--pSet :: PolySet
--pSet =
--    [ (cuboid, "a", ((5, 0, 0), (90, 0, 0), (1, 0.5, 1)))
--    , (cuboid, "b", ((-5, 0, 0), (90, 0, 0), (1, 0.5, 1))) ]

--------------------------------------------------------------------------------
---- | Write the commands to file.
--main :: IO ()
--main = do
--    let title = "basic"
--    let cmds = polySetToCmds pSet
--    writeFile (title ++ ".py") $ (preamble title ++ cmds)
--    return ()



main :: IO ()
main = do
    --let org = Polygon (genSphere 1 8) (identity 4) (0, 1, 0) (1, 0, 0) sphere
    --let b1 = Branch org Empty []
    --let bs = expand b1
    --let pSet = map (\b -> (node b)) (b1:bs)

    let org = updateLocs $ Polygon (genSphere 1 4) (identity 4) (0, 1, 0) (1, 0, 0) sphere
    let b1 = Branch org Empty []
    let pSet = map (\b -> (node b)) (recur 4 b1)

    putStrLn $ show $ length pSet
    --putStrLn $ show $ length (locations (pSet !! 0))
    --putStrLn $ show $ length (locations (pSet !! 1))
    --putStrLn $ show $ length (locations (pSet !! 2))
    --putStrLn $ show $ length (locations (pSet !! 3))
    --putStrLn $ show $ length (locations (pSet !! 4))



    let title = "basic"
    let cmds = polySetToCmds pSet
    writeFile (title ++ ".py") $ (preamble title ++ cmds)

    --putStrLn $ show (locations (node b1))
    --putStrLn $ show (length bs)
    return ()