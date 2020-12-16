module Main where

import System.Environment (getArgs)
import qualified Data.HashMap.Strict as H
import qualified Control.Applicative as A
import Text.Parsec (try, choice, eof, ParseError, anyChar, string, char, count, digit, letter, lookAhead, many, many1, manyTill, oneOf, parse, sepBy, sepEndBy)
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  content <- readFile "day7/data.txt"
  let eitherBags = doParse bags content
  case eitherBags of
    Right bags -> let task1 = solve1 $ H.fromList bags
                      --task2 = solve2 $ H.fromList bags
                  in putStrLn $ show task1
    Left err -> print err

-- Task 1
solve1 bags = findBags bags "shiny gold"
solve2 bags = findBagsDown bags (bags H.! "shiny gold")

findBags _ [] = H.empty
findBags bags name =
  let found = H.filter (elem name . map title) bags
  in found `H.union` findBags bags $ head $ H.keys found


doParse :: Parser a -> String -> Either ParseError a
doParse p s = parse p "" s
-- task 2
data Child = NoBag | Bag {title::String, val::Int} deriving (Show)

findBagsDown allBags list = H.foldr traverse 0 list
  where traverse Bag{val=val, title=title} acc = acc + val + val * findBagsDown allBags (allBags H.! title)
  
-- Parser
wordSep = char ' '
word = manyTill letter (oneOf " ,.\n")
name = do
  first <- word
  second <- word
  return (first ++ " " ++ second)

child = do
  size <- many1 digit
  _ <- wordSep
  name <- name
  _ <- word
  _ <- oneOf " \n"
  return $ Bag name (read size :: Int)

bags = many bag

noChild = do
  count 3 word
  try (char '\n')
  return []

bag = do
  name <- name
  count 2 word
  childs <- choice [many child, noChild]
  return (name, childs)
