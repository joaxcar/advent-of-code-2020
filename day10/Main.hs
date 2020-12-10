module Main where

import Data.List ( group, sort )

main :: IO ()
main = do
    input <- readFile "data.txt"
    let ints =  map read . lines $ input
    let jolts = 0 : sort ints ++ [maximum ints + 3]
    let task1 =  product . countDiffs . generateDiffs $ jolts
    let task2 = combinations . generateDiffs $ jolts
    print (task1, task2)

-- Task 1: calculate the product of amount of numbers with "diff 3" and "diff 1"
-- calculate the difference between all numbers and the subsequent number int the list
-- add a initial 0 for the charging outlet and a trailing 3 for the device 
generateDiffs :: Num a => [a] -> [a]
generateDiffs l = zipWith (-) (tail l) l

countDiffs :: Ord a => [a] -> [Int]
countDiffs = fmap length . group . sort

{- 
Task 2: calculate the number of possible combinations of adapters
The trick was to figure out that the number of combinations in a sublist is dependent on
the sequence of 1's between two 3's in the zippo output.
The pattern that emerges is
313
3113
31..13
If a one has a one to the left is is optional to leave it out. So the combinatorics becomes
2^(n-1) where n is the number of ones in the sequence. One then has to remove all cases when
three ones are lined up (if n is > 3). That is remove n - 3 cases.
-}
combinations :: [Int] -> Int
combinations = product . map (options . length) . filter (notElem 3) . group
    where options 1 = 1
          options 2 = 2
          options n = 2^(n-1) - (n - 3)