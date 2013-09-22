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
    let org = Polygon PolySphere (identity 4)
    let b1 = Branch org Empty []

    -- Recusrively expand it.
    let pSet = map (\b -> (node b)) (recur 5 b1)

    -- Write the python script
    let title = "basic"
    let cmds = polySetToCmds pSet
    writeFile (title ++ ".py") $ (preamble title ++ cmds)

    return ()

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------