import qualified Control.Monad.State as S
import Text.Parsec
import Text.Parsec.String (Parser)

main = do
  input <- readFile "Day12/data.txt"
  let prog = getRight $ parse cmds "" input
  let res = manhattan $ S.evalState (moveShip prog) (Ship (0, 0) 90)
  putStrLn $ show res

manhattan (x, y) = x + abs y

getRight (Right a) = a
getRight (Left _) = []

data Op = N | S | E | W | L | R | F deriving (Show, Eq, Ord)

data Cmd = Cmd {op :: Op, val :: Int} deriving (Show, Eq, Ord)

data Ship = Ship {pos :: (Int, Int), dir :: Int} deriving (Show, Eq, Ord)

type ShipState = (Ship, Op)

moveShip :: [Cmd] -> S.State Ship (Int, Int)
moveShip [] = do
  Ship pos _ <- S.get
  return pos
moveShip (cmd : rest) = do
  Ship {pos = (x, y), dir = d} <- S.get
  case op cmd of
    N -> S.put (Ship (x, y + val cmd) d)
    S -> S.put (Ship (x, y - val cmd) d)
    E -> S.put (Ship (x + val cmd, y) d)
    W -> S.put (Ship (x - val cmd, y) d)
    L -> S.put (Ship (x, y) (d - val cmd))
    R -> S.put (Ship (x, y) (d + val cmd))
    F -> S.put (move (Ship (x, y) d) (val cmd))
  moveShip rest

move (Ship (x, y) d) val = case d `mod` 360 of
  0 -> (Ship (x, y + val) d)
  180 -> (Ship (x, y - val) d)
  90 -> (Ship (x + val, y) d)
  270 -> (Ship (x - val, y) d)

-- parser
command = do
  op <- opType <$> oneOf "NSEWLRF"
  amount <- read <$> many1 digit
  return (Cmd op amount)

cmds = sepBy command (char '\n') <* eof

opType t
  | t == 'N' = N
  | t == 'S' = S
  | t == 'E' = E
  | t == 'W' = W
  | t == 'L' = L
  | t == 'R' = R
  | t == 'F' = F