import qualified Control.Monad.State as C
import qualified Control.Monad.State as S
import Data.Functor.Identity
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as M
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Printf (printf)

main = do
  input <- readFile "Day14/data.txt"
  let prog = getRight $ parse cmds "" input
  putStrLn . show $ runIt prog

noMask = map toTernary "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

applyMask [] _ = []
applyMask (m : ms) (b : bs) | m == MaskNone = b : applyMask ms bs
applyMask (m : ms) (b : bs) | m == MaskOne = One : applyMask ms bs
applyMask (m : ms) (b : bs) | m == MaskZero = Zero : applyMask ms bs

toDec l = foldl (\acc x -> if x == Zero then 2 * acc else 2 * acc + 1) 0 l

runIt k = C.evalState (runner k) (ProgState M.empty noMask)

getRight (Right a) = a
getRight (Left _) = []

toTernary x
  | x == '0' = MaskZero
  | x == '1' = MaskOne
  | x == 'X' = MaskNone

toBinary :: Char -> Binary
toBinary x
  | x == '0' = Zero
  | x == '1' = One

runner :: [Cmd] -> C.State ProgState Int
runner [] = do
  ProgState mem val <- C.get
  return $ sum . map toDec $ M.elems mem
runner ((BinData addr val) : res) = do
  ProgState mem mask <- C.get
  let newMem = M.insert addr (applyMask mask val) mem
  C.put $ ProgState newMem mask
  runner res
runner ((NewMask newMask) : res) = do
  ProgState mem _ <- C.get
  C.put $ ProgState mem newMask
  runner res

data ProgState = ProgState {mem :: IntMap Bin36, mask :: [Ternary]}

data Binary = Zero | One deriving (Show, Eq, Read, Ord, Bounded)

data Ternary = MaskZero | MaskOne | MaskNone deriving (Show, Eq)

type Bin36 = [Binary]

type Mask = [Ternary]

data Cmd = BinData {addr :: Int, val :: Bin36} | NewMask Mask deriving (Show, Eq)

-- parser

getMask = do
  string "ask = "
  m <- many1 (oneOf "X10")
  return (NewMask $ map toTernary m)

getMemory = do
  string "em["
  addr <- read <$> many1 digit
  string "] = "
  val <- (\x -> read x :: Int) <$> many1 digit
  return (BinData addr (map toBinary . printf "%036b" $ val))

command :: ParsecT String u Identity Cmd
command = do
  char 'm'
  getMask <|> getMemory

cmds :: ParsecT String u Identity [Cmd]
cmds = sepBy command (char '\n') <* eof