module Main where

main = do
  putStrLn "Enter filename of file containing the data:"
  fileName <- getLine
  contents <- readFile fileName
  let input = parse contents
  let result1 = solve $ listOfTwo input
  let result2 = solve $ listOfThree input
  putStrLn $ makeResultString result1 result2

makeResultString a b = "result: " ++ show a ++ " and " ++ show b

solve lst = product $ head $ filter ((== 2020) . sum) lst

parse text =  listToInt $ lines text

listToInt lst = map read lst :: [Int]

listOfTwo l = pure (\a b -> [a, b]) <*> l <*> l

listOfThree l = pure (\a b c -> [a, b, c]) <*> l <*> l <*> l