module Main where

main = do
  putStrLn "Enter filename of file containing the data:"
  fileName <- getLine
  contents <- readFile fileName
  putStrLn $ getResult contents

getResult = makeResultString . solve . listToInt . lines
  where 
    listToInt = map parseInt
    parseInt a = read a :: Int
    makeResultString (a, b) = "result: " ++ show a ++ " and " ++ show b

solve inData = (solve1 inData, solve2 inData)

solve1 = solveWith listOfTwo
  where listOfTwo l = pure (\a b -> [a, b]) <*> l <*> l

solve2 = solveWith listOfThree
  where listOfThree l = pure (\a b c -> [a, b, c]) <*> l <*> l <*> l

solveWith listGenerator = product . head . filter (is 2020 . sum) . listGenerator
  where
    is a = (== a) 