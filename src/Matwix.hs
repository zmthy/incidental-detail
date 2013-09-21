--------------------------------------------------------------------------------
-- Matwix, foundation for Hastwix library.
--
-- Provides simple matrix operations for working with n-dimensional row-sorted
-- lists (representing matrices).
-- 
-- Richard Roberts
--------------------------------------------------------------------------------

module Matwix where

import MatwixUtils

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Components a = [a]
type Shape = (Int, Int)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------
 
-- Create a matrix of zeros.
zeros :: Num a => Shape -> Components a
zeros dim = take ((fst dim) * (snd dim)) $ repeat 0 

-- Create an identity matrix.
identity :: Num a => Int -> Components a
identity n = map (flip match (n + 1) . floor) [0 .. components - 1]
  where components = fromIntegral (n * n)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- Gives a row of a matrix in the form of a list
row :: Num a => Int -> Shape -> Components a -> Components a
row _ _ [] = []
row i dim arr = take (snd dim) $ drop (i * (snd dim)) arr

-- Gives a column of a matrix in the form of a list
col :: Num a => Int -> Shape -> Components a -> Components a
col _ _ [] = []
col i dim arr = (arr !! i) : (col i dim $ drop (snd dim) arr)


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Modifiers
--------------------------------------------------------------------------------

-- Removes a row from a matrix
dropRow :: Num a => Int -> Shape -> Components a -> Components a
dropRow r (_, cDim) mtx = take (r * cDim) mtx ++ drop ((r + 1) * cDim) mtx

-- Removes a column from a matrix
dropCol :: Num a => Int -> Shape -> Components a -> Components a
dropCol _ _ [] = []
dropCol c dim mtx = arr ++ rArr
  where arr = (take c mtx) ++ (take ((snd dim) - 1 - c) $ (drop (c + 1) mtx))
        rArr = dropCol c dim (drop (snd dim) mtx)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- Reflects a matrix about its diagonal.
transpose :: Num a => Int -> Shape -> Components a -> Components a
transpose c dim mtx =
  if c == snd dim
     then []
     else (col c dim mtx) ++ (transpose (c + 1) dim mtx)

-- Performs a dot product over two 1-dimensional arrays.
dot1 :: Num a => [a] -> [a] -> a
dot1 a b = sum $ zipWith (*) a b

-- Performs a dot product over any two matrices, (columns in a must equal rows
-- in b).
dot :: Num a => Components a -> Components a -> Shape -> Shape -> Components a
dot a b dimA dimB = 
    if (snd dimA) == (fst dimB)
        then map dotFn [(r, c) | r <- [0..((fst dim) - 1)],
                                 c <- [0..((snd dim) - 1)]]
        else error "dot - can't dot a and b (dimensions don't match)"
    where dim = (fst dimA, snd dimB)
          dotFn = (\(r, c) -> dot1 (row r dimA a) (col c dimB b))

--------------------------------------------------------------------------------
