module Day6.Main where

import Data.List
import Data.List.Split
import qualified Data.Set as S
import AOC

main = do print . task2 <$> getInputLines "Day6/data.txt"

-- Task1: count the uniq options that all groups have chosen
task1 = sum . map (length . nub) . splitOn [""]
-- Task2: count only the options that all in the group have all chosen
task2 = sum . map ( S.size . foldr1 S.intersection . map S.fromList) . splitOn [""]