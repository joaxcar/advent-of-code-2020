module Day8.Main where

import Text.Parsec
import qualified Data.Sequence as S
import Text.Parsec.String (Parser)
import AOC
  
main = do
  input <- getInputRaw "Day8/data.txt"
  let prog = S.fromList $ getRight $ parse cmds "" input
  let task1 = acc . run1 . initP $ prog
  let task2 = flip S.index 0 $ S.filter (/= 0) $ allPos (run2 . initP) prog 
  print (task1, task2)

-- parser
op = count 3 anyChar
sep = char ' '
sign = oneOf "+-"

command = do
   op <- op
   sep
   sign <- sign
   amount <- read <$> many1 digit
   case sign of
      '-' -> return (op, -amount)
      '+' -> return (op, amount)
   
cmds = sepBy command (char '\n') <* eof


-- Task 1: run the program until loop  
run1 prog = let next = tick prog
  in if notUniq $ prev next then prog else run1 next

notUniq l = length l /= length (uniq l)

uniq [] = []
uniq (x:xs) = if x `elem` xs then uniq xs else x : uniq xs

data PState = PState {pos::Int, acc::Int, prev::[Int], prog::S.Seq (String, Int)} deriving (Show)

initP prog = PState 0 0 [] prog

getRight (Right a) = a
getRight (Left a) = []

tick state@(PState pos acc prev prog)=
  let op = prog S.!? pos
  in handle op
  where
    handle Nothing = PState (-1) acc (pos : prev) prog
    handle (Just ("jmp", n)) = PState (pos + n) acc (pos : prev) prog
    handle (Just ("nop", _)) = PState (succ pos) acc (pos : prev) prog
    handle (Just (_, n)) = PState (succ pos) (acc + n) (pos : prev) prog

-- Task 2: find a flipped operation and make the program run until the end. THIS implementation is SLOOOW!
-- might remake
run2 prog = let next = tick prog
  in if notUniq $ prev next then if 636 `elem` prev next then acc prog else 0 else run2 next

allPos f l = S.unfoldr change (0, l)
  where
    change (635, _) = Nothing
    change (n, l) = case l `S.index` n of
      ("nop", a) -> Just (f $ S.update n ("jmp", a) l, (n+1, l))
      ("jmp", a) -> Just (f $ S.update n ("nop", a) l, (n+1, l))
      (a, p) -> Just (0 , (n+1,l))