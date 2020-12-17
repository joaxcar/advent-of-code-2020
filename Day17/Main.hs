import Data.Maybe ()
import qualified Data.Set as S
import Data.List ( (\\) )

main :: IO ()
main = do
    input <-  concat . lines <$> readFile "Day17/data.txt"
    print . S.size . (!! 6) . iterate runner . initGrid $ map (\x -> case x of '.' -> 0; '#' -> 1) input

type Pos = (Integer , Integer , Integer, Integer)
parse :: [b] -> [(Pos, b)]
parse = zip [(0,0, x, y) | x <- [0..7], y <- [0..7]]
pos = [-1,0,1]
test = [(w, x, y, z) | w <- pos , x <- pos, y <- pos, z <- pos] \\ [(0,0,0,0)]

initGrid :: (Eq b, Num b) => [b] -> S.Set Pos
initGrid l = S.fromList . map fst . filter ((==1) . snd) $ parse l
check g p = let allPoss = map (addPos p) test
                checkPos = map (`S.member` g) allPoss
            in length $ filter (==True) checkPos
runner g = snd $ S.foldl tick (g, g) $ candidates g

candidates g = S.fromList $ addPos <$> (0,0,0,0):test <*> S.toList g
tick (g1, g2) p = let s = check g1 p 
           in if S.member p g1 then
               if s == 2 || s == 3 then (g1, g2) else (g1, S.delete p g2)
               else if s == 3 then (g1, S.insert p g2) else (g1, g2)

addPos (a, b, c, d) (w, x, y, z) = (a + w, b + x, c + y, d + z)


