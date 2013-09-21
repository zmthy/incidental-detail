--------------------------------------------------------------------------------
-- Hastwix, a Matrix library.
--
-- Provides dense matrices and the operations for working with them.
-- 
-- Richard Roberts
--------------------------------------------------------------------------------

module Hastwix (
  -- Types
  Matrix (Matrix), components, shape,

  -- Generating
  identity, row, col,

  -- Determinants
  determinant,

  -- Cofactors
  cofactors,

  -- Operations
  add, mult, transpose, dot, inverse)
where

import qualified Matwix as M
import MatwixUtils


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Matrix a = Matrix {
    components :: (M.Components a),
    shape :: M.Shape }
    deriving (Eq)

instance (Num a, Show a) => Show (Matrix a) where
    show (Matrix arr shape) = 
        "M.Components -\n" ++
        (unlines $ map (\i -> "  " ++ show (M.row i shape arr)) rows) ++
        "M.Shape - " ++ show shape
        where rows = [0 .. (fst shape) - 1]

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Generating
--------------------------------------------------------------------------------

identity :: Num a => Int -> Matrix a
identity i = Matrix (M.identity i) (i, i)

row :: Num a => Int -> Matrix a -> Matrix a
row i (Matrix arr shape) = Matrix (M.row i shape arr) (1, snd shape)

col :: Num a => Int -> Matrix a -> Matrix a
col i (Matrix arr shape) = Matrix (M.col i shape arr) (fst shape, 1)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Reduction
--------------------------------------------------------------------------------

dropRow :: Num a => Int -> Matrix a -> Matrix a
dropRow r (Matrix arr dim) = Matrix (M.dropRow r dim arr) (fst dim - 1, snd dim)

dropCol :: Num a => Int -> Matrix a -> Matrix a
dropCol c (Matrix arr dim) = Matrix (M.dropCol c dim arr) (fst dim, snd dim - 1)

reduce :: Num a => Int -> Int -> Matrix a -> Matrix a
reduce r c arr = dropRow r $ dropCol c arr

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Determinants
--------------------------------------------------------------------------------

det2x2 :: Num a => M.Components a -> a
det2x2 arr = arr !! 0 * arr !! 3 - arr !! 1 * arr !! 2

detSum :: Num a => Int -> M.Components a -> a
detSum _ [] = 0
detSum i (x:xs) =
  if i `mod` 2 == 0
       then (x) + rest
       else (-x) + rest
  where rest = detSum (i + 1) xs

determinant :: Num a => Matrix a -> a
determinant (Matrix arr (2, 2)) = det2x2 arr
determinant m@(Matrix arr (r, c)) = detSum 0 $ map fn [0 .. c - 1]
  where fn = (\i -> (r !! i) * determinant (reduce 0 i m))
        r = components (row 0 m)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cofactors
--------------------------------------------------------------------------------

cofactorSign :: Num a => Int -> Int -> a
cofactorSign a b =
  if (isEven a) == (isEven b)
    then 1
    else -1

cofactors :: Num a => Matrix a -> Matrix a
cofactors m@(Matrix arr dim@(r, c)) = Matrix (M.transpose 0 dim arr) dim
  where fn = (\(x, y) -> (cofactorSign x y) * (determinant (reduce x y m)))
        arr = map fn [(x, y) | x <- [0 .. r - 1], y <- [0 .. c - 1]]

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Row Operations
--------------------------------------------------------------------------------

setRow :: Num a => Int -> M.Components a -> Matrix a -> Matrix a
setRow i row (Matrix arr shape) = Matrix (pred ++ row ++ succ) shape
    where pred = take (i * (snd shape)) arr
          succ = drop ((i + 1) * (snd shape)) arr

rowExchange :: Num a => Int -> Int -> Matrix a -> Matrix a
rowExchange a b arrA = setRow a rB (setRow b rA arrA)
    where rA = components (row a arrA)
          rB = components (row b arrA)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Matrix Operations
--------------------------------------------------------------------------------

add :: Num a => Matrix a -> Matrix a -> Matrix a
add (Matrix a1 _) (Matrix a2 dim) = Matrix (zipWith (+) a1 a2) dim

mult :: Num a => a -> Matrix a -> Matrix a
mult scalar (Matrix arr dim) = Matrix (map (scalar*) arr) dim

transpose :: Num a => Matrix a -> Matrix a
transpose (Matrix arr dim) = (Matrix (M.transpose 0 dim arr) newDim)
  where newDim = (snd dim, fst dim)

dot :: Num a => Matrix a -> Matrix a -> Matrix a
dot (Matrix arrA shapeA) (Matrix arrB shapeB) = Matrix ab shape
    where ab = M.dot arrA arrB shapeA shapeB
          shape = (fst shapeA, snd shapeB)

inverse :: Fractional a => Matrix a -> Matrix a
inverse m@(Matrix arr dim@(r, c)) =
  if r == c
    then mult (1 / det) cofs
    else error "inverseMtx - matrix not square!" 
  where det = determinant m
        cofs = cofactors m

--------------------------------------------------------------------------------
