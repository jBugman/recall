module Main where

main :: IO ()
main = print $ mergeSort (<=) testList

testList :: [Int]
testList = [1, 4, 8, 3, 2, 0, 1, 5, 2, 7, 4]

type Predicate a = a -> a -> Bool

mergeSort :: Predicate a -> [a] -> [a]
mergeSort _ []  = []
mergeSort _ [x] = [x]
mergeSort p xs  = merge p (mergeSort p ys) (mergeSort p zs)
  where (ys, zs) = split xs

split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:xs) = (x:ys, zs)
  where (zs, ys) = split xs

merge :: Predicate a -> [a] -> [a] -> [a]
merge _ xs [] = xs
merge _ [] xs = xs
merge p (x:xs) (y:ys)
  | p x y     = x : merge p xs (y:ys)
  | otherwise = y : merge p (x:xs) ys
