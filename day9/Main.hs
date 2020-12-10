import Data.List (inits, tails, unfoldr)

main :: IO ()
main = do
  input <- readFile "data.txt"
  let nums = map (\x -> read x :: Int) $ lines input
  let invalid = snd . head $ findInvalid 25 nums
  let weakness = findWeakness invalid nums
  print (invalid, weakness)

-- Task 1
initXmas :: Int -> [a] -> ([a], [a])
initXmas n l = (reverse $ take n l, drop n l)

findInvalid :: (Eq b, Num b) => Int -> [b] -> [([b], b)]
findInvalid preamble sequence = filterInvalid $ steps preamble $ initXmas preamble sequence

steps :: Int -> ([a], [a]) -> [([a], a)]
steps preamble = unfoldr step
  where
    step (_, []) = Nothing
    step (p, x : xs) = Just ((take preamble p, x), (x : p, xs))

filterInvalid :: (Eq b, Num b) => [([b], b)] -> [([b], b)]
filterInvalid xSteps = filter invalid xSteps
  where
    invalid = not . valid

valid :: (Eq b, Num b) => ([b], b) -> Bool
valid (preamble, val) = any check $ tails preamble
  where
    check [] = False
    check (y : ys) = (val - y) `elem` ys

-- Task 2
findWeakness :: (Num c, Ord c) => c -> [c] -> c
findWeakness invalid = weakness . head . getWeakSequence
  where
    getWeakSequence = filter ((==) invalid . sum) . getAllSequencesTill invalid

getAllSequencesTill :: Eq a => a -> [a] -> [[a]]
getAllSequencesTill num = concatMap tails . inits . reverse . takeWhile (/= num)

weakness :: (Num a, Foldable t, Ord a) => t a -> a
weakness range = maximum range + minimum range