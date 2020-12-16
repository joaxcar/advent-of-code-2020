module Day11.Main where

import AOC
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

main = do
    input <- M.fromList . toPair <$> map (M.fromList . toPair) <$> map (map tr) <$> getInputLines "Day11/ex.txt"
    putStrLn $ show $ input

n = (0, 1)
ne = (1, 1)
e = (1, 0)
se = (1, -1)
s = (0, -1)
sw = (-1, -1)
w = (-1, 0)
nw = (-1, 1)
dirs = [n, ne, e, se, s, sw, w ,nw]

!? =
getPos (x, y) = wrap $ M.lookup x =<< M.lookup y


data Seat = Floor | Free | Occupied deriving (Show, Eq)
newtype Grid a = Grid {toMap :: IntMap (IntMap a)}
fromMap :: IntMap (IntMap a) -> Grid a
fromMap m = Grid m
wrap f = fromMap . f . toMap

instance Functor Grid where
    fmap f = wrap $ M.map (M.map f)

travel grid start dir = let cur = addto start dir
                                      in case getPos grid cur of
                                          Just Floor -> travel grid cur dir
                                          Just a -> a
                                          Nothing -> Floor

addto (a, b) (x, y) = (a + x, b + y)
paddingBF :: [Char] -> [Char]
paddingBF [] = []
paddingBF x = '.' : x ++ ['.']
paddingTB :: [[Char]] -> [[Char]]
paddingTB [] = []
paddingTB l@(x:_) = let extra = ['.' | _ <- x]
                 in extra : l ++ [extra]

toPair l = zip [0..length l] l
tr '.' = Floor
tr 'L' = Free
tr '#' = Occupied
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

