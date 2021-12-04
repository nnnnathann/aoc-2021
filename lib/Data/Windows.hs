module Data.Windows where

{-
  Operations supporting windows of lists
-}

window2 :: [a] -> [(a, a)]
window2 xs = init $ zip xs (rotate xs)

window3 :: [a] -> [(a, a, a)]
window3 xs = init $ init (zip3 xs (rotate xs) (rotate (rotate xs)))

-- Rotate shifts an array to the right, for example
-- [2,3,4,1] -> [1,2,3,4]
rotate :: [a] -> [a]
rotate [] = []
rotate xs = cycle (drop 1 xs)