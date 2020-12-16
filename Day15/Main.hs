import Data.IntMap (IntMap)
import qualified Data.IntMap.Lazy as M
import Data.List (foldl', group, sort, unfoldr)

main :: IO ()
main = print . fst $ generator3 30000000 input1

input1 :: [Int]
input1 = [20, 9, 11, 0, 1, 2]

inputs = [[0, 3, 6], [2, 1, 3], [3, 1, 2], [2, 3, 1], [1, 3, 2], [3, 2, 1]]

doit = map (sum . take 30 . generator) inputs

gameInit inp = foldr (uncurry M.insert) M.empty (inp `zip` [1, 2 ..])

generator input = init input ++ unfoldr go (gameInit input, last input, length input)
  where
    go (gameMap, lastSeen, index) =
      case gameMap M.!? lastSeen of
        Just pos ->
          let num = index - pos
           in Just (lastSeen, (M.insert lastSeen index gameMap, num, succ index))
        Nothing ->
          Just (lastSeen, (M.insert lastSeen index gameMap, 0, succ index))

generator2 stop input = go (gameInit $ init input, last input, length input, stop)
  where
    go (_, l, _, 0) = l
    go (gameMap, lastSeen, index, stop) =
      case gameMap M.!? lastSeen of
        Just pos ->
          let num = index - pos
           in go (M.insert lastSeen index gameMap, num, succ index, stop - 1)
        Nothing ->
          go (M.insert lastSeen index gameMap, 0, succ index, stop - 1)

generator3 stop input = foldl' go (last input, gameInit $ init input) [(length input) .. stop - 1]
  where
    go (last, m) i = case M.insertLookupWithKey f last i m of
      (Just a, newMap) -> (i - a, newMap)
      (Nothing, newMap) -> (0, newMap)
    f _ new_value old_value = new_value