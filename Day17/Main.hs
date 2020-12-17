import Data.IntMap (IntMap)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import Data.List

main = do
    input <-  concat . lines <$> readFile "Day17/data.txt"
    putStrLn . show . sumGrid . (!! 6) . iterate runner . initGrid $ map (\x -> case x of '.' -> 0; '#' -> 1) input

parse l = zip [(0, x, y) | x <- [0..7], y <- [0..7]] l
maxi = 14
pos = [-1,0,1]
pos2 = [-8..maxi]
test = [(x, y, z) | x <- pos, y <- pos, z <- pos] \\ [(0,0,0)]
allPos = [(x, y, z) | x <- pos2, y <- pos2, z <- pos2]
initGrid l = foldl (uncurry . updatePos2) g2 $ parse l
check g p = let allPoss = map (addPos p) test
                checkPos = mapMaybe (getPos2 g) allPoss
            in sum checkPos
sumGrid g = sum $ mapMaybe (getPos2 g) allPos
runner g = snd $ foldl tick (g, g) allPos
tick (g1, g2) p = let s = check g1 p 
           in case getPos2 g1 p of
               Nothing -> (g1, g2)
               Just a -> case a of
                   1 -> if s == 2 || s == 3 then (g1, g2) else (g1, updatePos2 g2 p 0)
                   0 -> if s == 3 then (g1, updatePos2 g2 p 1) else (g1, g2)

addPos (a, b, c) (x, y, z) = (a + x, b + y, c + z)
--zz = IM.fromList [(x, 0) | x <- [0..maxi]]
--yy = IM.fromList [(x, zz) | x <- [0..maxi]]
--xx = IM.fromList [(x, yy) | x <- [0..maxi]]

--g11 = xx

g2 = M.fromList $ zip allPos $ repeat 0
--type Grid = IntMap (IntMap (IntMap Int))
--mapGrid g f = IM.map (IM.map (IM.map f)) g

--getPos m (x, y, z) = m IM.!? x >>= \x -> x IM.!? y >>= \x -> x IM.!? z
getPos2 m p = M.lookup p m
updatePos2 m p v = M.insert p v m
--updatePos m (x, y, z) v = IM.insert x ( IM.insert y (IM.insert z v zMap) yMap) xMap
  --  where xMap = m
    --      yMap = xMap IM.! x
      --    zMap = yMap IM.! y


