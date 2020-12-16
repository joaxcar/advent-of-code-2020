import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as M
import Text.Parsec
  ( digit,
    eof,
    many1,
    oneOf,
    parse,
    sepBy,
    string,
    try,
  )
import Text.Printf (printf)

main = do
  input <- readFile "data.txt"
  let prog = getRight $ parse cmds "" input
  print . task2 . M.unions . reverse $ map (mem . runner) prog

-- Modeling the program
data ProgState = ProgState {mem :: IntMap Bin36, mask :: Mask}

data Binary = Zero | One deriving (Show, Eq, Read, Ord, Bounded)

data Ternary = MaskZero | MaskOne | MaskNone deriving (Show, Eq)

type Bin36 = [Binary]

type Mask = [Ternary]

data Cmd = BinData {addr :: Int, val :: Bin36} deriving (Show, Eq)

task2 :: IntMap [Binary] -> Integer
task2 = sum . map toDec . M.elems

runner :: Foldable t => (Mask, t Cmd) -> ProgState
runner (mask, l) = foldl runns (ProgState M.empty mask) l
  where
    runns (ProgState mem m) (BinData addr val) =
      let addrs = applyMask m (map toBinary $ intToBinString addr)
          add x acc = M.insert (toDec x) val acc
          newMem = foldr add mem addrs
       in ProgState newMem mask

runner1 (mask, l) = foldl runns (ProgState M.empty mask) l
  where
    runns (ProgState mem mask) (BinData addr val) =
      let newMem = M.insert addr (applyMask mask val) mem
       in ProgState newMem mask

applyMask :: [Ternary] -> [Binary] -> [[Binary]]
applyMask m b = applyMask' m b [[]]

applyMask' :: [Ternary] -> [Binary] -> [[Binary]] -> [[Binary]]
applyMask' [] _ a = a
applyMask' (m : ms) (b : bs) acc
  | m == MaskNone = applyMask' ms bs ((:) <$> [One, Zero] <*> acc)
  | m == MaskOne = applyMask' ms bs ((:) <$> [One] <*> acc)
  | m == MaskZero = applyMask' ms bs ((:) <$> [b] <*> acc)

toDec :: (Foldable t, Num a) => t Binary -> a
toDec = foldl (\acc x -> if x == Zero then 2 * acc else 2 * acc + 1) 0

getRight :: Either a1 [a2] -> [a2]
getRight (Right a) = a
getRight (Left _) = []

toTernary :: Char -> Ternary
toTernary x
  | x == '0' = MaskZero
  | x == '1' = MaskOne
  | x == 'X' = MaskNone

toBinary :: Char -> Binary
toBinary x
  | x == '0' = Zero
  | x == '1' = One

intToBinString n = printf "%036b" n :: String

-- parser

getMask = do
  string "mask = "
  m <- many1 (oneOf "X10")
  return (map toTernary m)

getMemory = do
  string "["
  addr <- read <$> many1 digit
  string "] = "
  val <- (\x -> read x :: Int) <$> many1 digit
  return (BinData addr (map toBinary . intToBinString $ val))

command = do
  mask <- getMask
  string "\nmem"
  mems <- sepBy getMemory (try (string "\nmem"))
  return (mask, mems)

cmds = sepBy command (string "\n") <* eof