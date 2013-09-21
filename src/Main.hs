------------------------------------------------------------------------------
module Main where

------------------------------------------------------------------------------
-- | Source
import Maya


import Text.Printf


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


showTriple :: Point -> IO ()
showTriple p = putStrLn $ printf "x:%2.2f, y:%2.2f, z:%2.2f" (px p) (py p) (pz p)

main :: IO ()
main = do
    --let org = Polygon (genSphere 1 8) (identity 4) (0, 1, 0) (1, 0, 0) sphere
    --let b1 = Branch org Empty []
    --let bs = expand b1
    --let pSet = map (\b -> (node b)) (b1:bs)

    let org = updateLocs $ Polygon (unique (genSphere 1 4)) (identity 4) (0, 1, 0) (1, 0, 0) sphere
    let b1 = Branch org Empty []
    let pSet = map (\b -> (node b)) (recur 5 b1)


    --putStrLn $ show $ length (locations (pSet))

    --showTriple $ fst $ (locations (pSet !! 0)) !! 0
    --showTriple $ fst $ (locations (pSet !! 0)) !! 1
    --showTriple $ fst $ (locations (pSet !! 0)) !! 2
    --showTriple $ fst $ (locations (pSet !! 0)) !! 3

    --showTriple $ fst $ (locations (pSet !! 0)) !! 4
    --showTriple $ fst $ (locations (pSet !! 0)) !! 5
    --showTriple $ fst $ (locations (pSet !! 0)) !! 6
    --showTriple $ fst $ (locations (pSet !! 0)) !! 7

    --showTriple $ fst $ (locations (pSet !! 0)) !! 8
    --showTriple $ fst $ (locations (pSet !! 0)) !! 9
    --showTriple $ fst $ (locations (pSet !! 0)) !! 10
    --showTriple $ fst $ (locations (pSet !! 0)) !! 11

    --showTriple $ fst $ (locations (pSet !! 0)) !! 12
    --showTriple $ fst $ (locations (pSet !! 0)) !! 13
    --showTriple $ fst $ (locations (pSet !! 0)) !! 14
    --showTriple $ fst $ (locations (pSet !! 0)) !! 15



    



    let title = "basic"
    let cmds = polySetToCmds pSet
    writeFile (title ++ ".py") $ (preamble title ++ cmds)

    --putStrLn $ show (locations (node b1))
    --putStrLn $ show (length bs)
    return ()