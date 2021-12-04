module Data.Windows where

window2 :: [a] -> [(a, a)]
window2 xs = init $ zip xs (rotate xs)

window3 :: [a] -> [(a, a, a)]
window3 xs = init $ init (zip3 xs (rotate xs) (rotate (rotate xs)))

rotate :: [a] -> [a]
rotate [] = []
rotate xs = cycle (drop 1 xs)