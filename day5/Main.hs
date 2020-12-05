module Main where

import Data.List (sortBy)
import System.Environment (getArgs)

main :: IO ()
main = do
  filename <- getArgs
  content <- readFile $ head filename
  let ls = lines content
  let res1 = head $ listIDs ls
  let res2 = head $ findMySeat $ listIDs ls
  print (res1, res2)

-- part 1
listIDs :: [[Char]] -> [ID]
listIDs lst = revSort $ map (calculateID . parseSeat) lst

data Seat = Seat {row :: Int, col :: Int} deriving (Show)

type ID = Int

parseSeat :: [Char] -> Seat
parseSeat str = Seat (findRow $ take 7 str) (findCol $ drop 7 str)

calculateID :: Seat -> ID
calculateID s = row s * 8 + col s

findRow :: [Char] -> Int
findRow = findPos (0, 127)

findCol :: [Char] -> Int
findCol = findPos (0, 7)

findPos :: (Foldable t, Integral b) => (b, b) -> t Char -> b
findPos init s = fst $ foldl bipar init s
  where
    bipar (a, b) n =
      case n of
        'F' -> (a, b - 1 - div (b - a) 2)
        'L' -> (a, b - 1 - div (b - a) 2)
        'B' -> (a + 1 + div (b - a) 2, b)
        'R' -> (a + 1 + div (b - a) 2, b)

revSort :: [ID] -> [ID]
revSort = sortBy (flip compare)

-- part 2, the data we have is from 21 to 671
-- we are not in the front or back so in this range
findMySeat :: Foldable t => t ID -> [ID]
findMySeat lst = filter (\x -> not (x `elem` lst)) allSeats

allSeats :: [ID]
allSeats = [21 .. 996]
