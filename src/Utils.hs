--------------------------------------------------------------------------------
-- | Incidental Detail, a library for generating details of the incidental kind.

module Utils where

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Specifies whether both inputs are even, or both are odd.
match :: Num a => Int -> Int -> a
match a b
  | a `mod` b == 0 = 1
  | otherwise      = 0

--------------------------------------------------------------------------------
-- | Specifies whether the given number is even.
isEven :: Int -> Bool
isEven a = a `mod` 2 == 0

--------------------------------------------------------------------------------
-- | Merges two lists in the fashion of [x1, y1, x2, y2, x3, y3, ... xN, yN]
merge :: Num a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

--------------------------------------------------------------------------------
-- | Repeats each element in a list the given number of times.
repeatElems :: [Double] -> Int -> [Double]
repeatElems [] _ = []
repeatElems (x:xs) repeats = repeatHead ++ (repeatElems xs repeats)
    where repeatHead = (take repeats $ (repeat x))

--------------------------------------------------------------------------------
-- | Repeats list the given number of times.
repeatList :: [Double] -> Int -> [Double]
repeatList _ 0 = []
repeatList arr repeats = arr ++ (repeatList arr (repeats - 1))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------