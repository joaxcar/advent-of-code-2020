module Main where

import System.Environment (getArgs)
import Text.Parsec (ParseError, anyChar, char, count, digit, letter, lookAhead, many, many1, manyTill, oneOf, parse, sepBy, sepEndBy)
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  filename <- getArgs
  content <- readFile $ head filename
  let res = case (doParse passports content) of
        Left _ -> (0, 0)
        Right x -> (countTrue $ map validate1 x, countTrue $ map validate2 x)
  putStrLn (show res)

doParse :: Parser a -> String -> Either ParseError a
doParse p s = parse p "" s

-- Task 1
validate1 input = all (== True) $ pure elem <*> fields <*> (pure $ map fst input)

fields =
  ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

countTrue a = foldr (\x n -> if x == True then n + 1 else n) 0 a

-- Parser
idSeparator = char '\n'

fieldSeparator = oneOf " \n"

keyvalSeparator = char ':'

passports = sepBy passport idSeparator

passport = sepEndBy field fieldSeparator

field = do
  a <- many1 letter
  _ <- keyvalSeparator
  b <- manyTill anyChar (lookAhead fieldSeparator)
  return (a, b)

-- Task 2
validate2 input =
  if validate1 input
    then all (== True) $ map (isValidField . parseField) input
    else False

isValidField a = case a of
  Left _ -> False
  Right a -> a

betweenNums n l u = (n >= l) && (n <= u)

-- parser
year = count 4 digit

byr = do
  y <- year
  if betweenNums (read y :: Int) 1920 2002
    then return True
    else return False

iyr = do
  y <- year
  if betweenNums (read y :: Int) 2010 2020
    then return True
    else return False

eyr = do
  y <- year
  if betweenNums (read y :: Int) 2020 2030
    then return True
    else return False

hgt = do
  h <- many digit
  d <- count 2 letter
  if (d == "cm" && betweenNums (read h :: Int) 150 193) || (d == "in" && betweenNums (read h :: Int) 59 76)
    then return True
    else return False

hcl = do
  _ <- char '#'
  _ <- count 6 (oneOf "1234567890abcdef")
  d <- many anyChar
  if (length d) > 0 then return False else return True

ecl = do
  str <- count 3 letter
  d <- many anyChar
  if str `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] || (length d) > 0
    then return True
    else return False

pid = do
  _ <- count 9 digit
  d <- many anyChar
  if (length d) > 0 then return False else return True

parseField (a, b) = case a of
  "pid" -> doParse pid b
  "ecl" -> doParse ecl b
  "hcl" -> doParse hcl b
  "hgt" -> doParse hgt b
  "eyr" -> doParse eyr b
  "iyr" -> doParse iyr b
  "byr" -> doParse byr b
  "cid" -> Right True