module Day11.Main where

import AOC
import Data.List

main = do
    input <- map (map tr) <$> paddingTB <$> map paddingBF <$> getInputLines "Day11/data.txt"
    let test = countOc $ cont start input
    putStrLn $ show test

paddingBF :: [Char] -> [Char]
paddingBF [] = []
paddingBF x = '.' : x ++ ['.']
paddingTB :: [[Char]] -> [[Char]]
paddingTB [] = []
paddingTB l@(x:_) = let extra = ['.' | _ <- x]
                 in extra : l ++ [extra]

tr '.' = 0
tr 'L' = 1
tr '#' = 10
cont f l = if f l == l then l else cont f (f l)
sumThree x y z = x + y + z

start (x:xs) = x : runner (x:xs)
runner l@(a:b:c:_) = (0: run (tail b) (zipWith3 sumThree a b c)) : runner (tail l)
runner (a:b:res) = b : runner res
runner _ = []

run (1:xs) l@(a:b:c:_) = if sumThree a b c < 10 then 10 : run xs (tail l) else 1 : run xs (tail l)
run (10:xs) l@(a:b:c:_) = if sumThree a b c >= 50 then 1 : run xs (tail l) else 10 : run xs (tail l)
run (0:xs) l@(a:b:c:_) = 0 : run xs (tail l)
run _ _ = [0]

countOc l = length $ filter (==10) $ concat l 

expand (a,b) ll = 