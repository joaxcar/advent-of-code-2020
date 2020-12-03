module Main where
import System.Environment ( getArgs )

{-
A bit of a weird solution to this problem. The program is generating a list of all moves by a pattern.
Then checks this list against the text. The reason for this is pre optimization. I thought that the
second part of the puzzle might involve a more intricate pattern (moving upwards or similar). It was
not needed...
Anyways it was a fun exercise in using unfold.
-}
main :: IO ()
main = do
    filename <- getArgs
    content <- readFile $ head filename
    let text = lines content
    let result1 = countTrees text $ generateMoves (length text) (head allPatterns)
    let result2 = product $ map (countTrees text) $ map (generateMoves $ length text) allPatterns
    putStrLn ("====AoC day 3====")
    putStrLn ("Result 1: " ++ (show result1))
    putStrLn ("Result 2: " ++ (show result2))

countTrees text moves = count '#' $ map getValue moves
    where getValue (x, y) = getAtIndex x (getAtIndex y text)

-- Generate the list of all points touched by all patterns
generateMoves length pattern = unfold (moveByPattern pattern length) (0,0)

-- The relevant patterns
data Pattern = Pattern {onX::Int->Int, onY::Int->Int}
allPatterns = [Pattern ((flip mod) 31 . (+3)) (+1)
          , Pattern ((flip mod) 31 . (+1)) (+1)
          , Pattern ((flip mod) 31 . (+5)) (+1)
          , Pattern ((flip mod) 31 . (+7)) (+1)
          , Pattern ((flip mod) 31 . (+1)) (+2)]

moveByPattern Pattern{onX=moveX, onY=moveY} end point@(x, y) = 
    if y >= end 
        then Nothing 
        else Just (point, (moveX x, moveY y))

count :: Eq a => a -> [a] -> Int
count t l = length $ filter (== t) l

getAtIndex :: Int -> [a] -> a
getAtIndex i str = head $ drop i str

unfold :: (b -> Maybe (a, b)) -> b -> [a]
unfold f val = case f val of
       Just (a, b) -> a : unfold f b 
       Nothing -> []