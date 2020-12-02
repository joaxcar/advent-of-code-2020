module Main where
import System.Environment ( getArgs )

main :: IO ()
main = do
    filename <- getArgs
    content <- readFile $ head filename
    let res1 = sum $ checkWith validate1 $ lines content
    let res2 = sum $ checkWith validate2 $ lines content
    putStrLn ("Result: " ++ show res1 ++ " and " ++ show res2)

data PassWithPolicy = None | Pwd { range :: (Int, Int)
                                 , token :: Char
                                 , password :: String} 
                                 deriving (Show, Eq)

checkWith :: (PassWithPolicy -> b) -> [[Char]] -> [b]
checkWith f = map (f . parse)

parse :: [Char] -> PassWithPolicy
parse s = passWithPolicyGenerator $ splitOn ' ' s

-- This here is bad. Should check the data more than just that there are three parts
passWithPolicyGenerator :: [[Char]] -> PassWithPolicy
passWithPolicyGenerator (x:y:z:_) = Pwd (getRange x) (head y) z
    where getRange s = (\(x:y:_) -> (x, y)) $ map (\n -> read n::Int) $ splitOn '-' s
passWithPolicyGenerator _ = None


validate1 :: Num p => PassWithPolicy -> p
validate1 None = 0
validate1 Pwd {token=t, range=r, password=pwd} = 
    let n = count t pwd
    in if inRange r n then 1 else 0

validate2 :: Num p => PassWithPolicy -> p
validate2 None = 0
validate2 Pwd {token=t, range=r, password=pwd} = 
    let upper = snd r
        lower = fst r
    in if xor (isAtIndex t lower pwd) (isAtIndex t upper pwd) then 1 else 0

count :: (Foldable t, Num b, Eq a) => a -> t a -> b
count t = foldr match 0
    where match x n = if x == t 
                      then n + 1
                      else n

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

isAtIndex :: Eq a => a -> Int -> [a] -> Bool
isAtIndex c i str = let rem = drop (i - 1) str
                    in case rem of
                        [] -> False
                        (x:_) -> x == c

inRange :: Ord a => (a, a) -> a -> Bool
inRange r n = let upper = snd r
                  lower = fst r
                in
                    n >= lower && n <= upper

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str = case break (== c) str of
    (str, []) -> [str]
    (str, rest) -> str : splitOn c (drop 1 rest)
