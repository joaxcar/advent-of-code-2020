import Control.Monad.State
import Data.List (sortOn)
import Data.Ord (Down (Down))

main :: IO ()
main = do
  let k = sortTups l
  let s = evalState (calcTimestamp k) (1, uncurry (+) (head k) + 1)
  print s

l :: [(Int, Int)]
l = [(17, 0), (41, 7), (937, 17), (13, 9), (23, 17), (29, 17), (397, 48), (37, 17), (19, 10)]

-- l = [(5, 3), (3, 2), (7, 1)]

sortTups :: [(Int, b)] -> [(Int, b)]
sortTups = sortOn (Down . fst)

calcTimestamp :: [(Int, Int)] -> State (Int, Int) Int
calcTimestamp [] = do
  (primeProduct, total) <- get
  return $ abs $ primeProduct - total
calcTimestamp list@((prime, remainder) : res) = do
  (incrementor, total) <- get
  if total `mod` prime == remainder
    then
      put (incrementor * prime, total)
        >> calcTimestamp res
    else
      put (incrementor, total + incrementor)
        >> calcTimestamp list
