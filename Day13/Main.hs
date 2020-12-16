import Control.Monad.State
import Data.List
import Data.Ord
--l = [(7, 1), (3,2), (11,3)]
--l = [(67,1),(7,2),(59,3),(61,4)]
--l = [(1789,1),(37,2),(47,3),(1889,4)]

--l = [(17,0),(41,1),(938,1),(13,36),(23,41),(29,47),(397,49),(37,55),(19,68)]
l = [(17,0),(41,7),(937,17),(13,9),(23,17),(29,17),(397,48),(37,17),(19,10)]
--k= sortBy (comparing $ Down . fst) l

doIt :: [(Int, Int)] -> State (Int, Int) Int
doIt [] = do
  (incrementer, total) <- get
  return (abs ( incrementer - total)) 
doIt list@((prime, remainder):res) = do
  (incrementer, total) <- get
  if total `mod` prime == remainder
    then 
      put (incrementer * prime, total) >>
      doIt res
    else
      put (incrementer, total + incrementer) >>
      doIt list
      
main = do print $ evalState (doIt l) (1, 0)
