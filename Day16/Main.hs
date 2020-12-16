import AOC
import Data.Ord

main = do
    input <- getInputRaw "Day16/data.txt"
    let ins =  head . getRight . parse parseIn $ input
    print (task1 ins, task2 ins) 

-- model
data Setup = Setup { myTicket :: Ticket
                   , nearbyTickets :: [Ticket]
                   , rules :: [Rule]} deriving (Show, Eq)
type Ticket = [Int]
data Rule = Rule { name :: String
                 , this :: Range
                 , that :: Range} deriving (Show, Eq)
data Range = Range { low :: Int
                   , high :: Int} deriving (Show, Eq)

-- task 1
task1 (Setup _ nearby rules) = sum $ concatMap (`invalid` rules) nearby

invalid ticket rules = filter (not . check rules) ticket

check rules num = any (validate num) rules

validate :: Int -> Rule -> Bool
validate num rule = num `inRange` this rule || num `inRange` that rule

inRange :: Int -> Range -> Bool
inRange num range = low range <= num && high range >= num 
-- 
-- task 2
task2 (Setup my nearby rules) = product
                                . map ((!!) my . fst)
                                . filter (isPrefixOf "departure" . snd)
                                . identifyRules rules 
                                $ filterValid rules nearby

filterValid rules = filter (all (check rules))

identifyRules rules tickets = findIndexOfRule . nameOfUsedRule $ tickets
    where
        nameOfUsedRule tickets = map (map findName) tickets
        findName num = map name $ possibleRules num
        possibleRules num = filter (validate num) rules
        findIndexOfRule = sortOn fst . uniq . sortOn (length.snd) . zip [0..] . map (foldr1 intersect) . transpose
            where
                uniq [] = []  
                uniq ((i, l):ls) = (i, head l):uniq (map (fmap ( \\ l)) ls)


-- parsing
range = do
    lower <- read <$> many1 digit
    char '-'
    upper <- read <$> many1 digit
    return $ Range lower upper


rule = do
    name <- manyTill anyChar (string ": ")
    rule1 <- range
    string " or "
    rule2 <- range
    newline
    return $ Rule name rule1 rule2

ticket = do
    nums <- sepBy (many1 digit) (char ',')
    return $ map read nums


parseIn = do
    rules <- manyTill rule newline
    string "your ticket:\n"
    my <- ticket
    count 2 newline
    string "nearby tickets:\n"
    nearby <- sepBy ticket newline <* eof
    return $ [Setup my nearby rules]



