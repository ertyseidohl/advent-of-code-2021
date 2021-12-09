module Main where

import Debug.Trace (trace)
import Data.List.Split ( splitOn )
import Data.List (transpose)

newtype Board = Board [Five] deriving (Show)
newtype Five = Five (Int, Int, Int, Int, Int) deriving (Show)

getRawInput :: IO String
getRawInput = readFile "./src/day04/input"

splitNumbers :: String -> [Int]
splitNumbers xs = map read $ splitOn "," xs

processLine :: String -> [Int]
processLine "" = []
processLine (' ':xs) = processLine xs
processLine xs = read (takeWhile (/= ' ') xs) : processLine (dropWhile (/= ' ') xs)

fiveFromList :: [Int] -> Five
fiveFromList [a, b, c, d, e] = Five (a, b, c, d, e)
fiveFromList n = error $ "A Five must have five values " ++ show n

createBoard :: [String] -> Board
createBoard ("":xs) = createBoard xs
createBoard xs = createBoard' (map processLine xs)

createBoard' :: [[Int]] -> Board
createBoard' xs = Board (rows ++ cols) where
  rows = map fiveFromList xs
  cols = map fiveFromList (transpose xs)

splitBoards :: [String] -> [Board]
splitBoards [] = []
splitBoards xs = createBoard (take 6 xs) : splitBoards (drop 6 xs)
 
parseInput :: [String] -> ([Int], [Board])
parseInput xs = (splitNumbers $ head xs , splitBoards $ tail xs)

scratch :: Int -> Five -> Five
scratch n (Five (a, b, c, d, e)) = Five (z a, z b, z c, z d, z e) where
  z = \e -> if e == n then -1 else e

processBoard :: Int -> Board -> Board
processBoard n (Board board) = Board $ map (scratch n) board

allScratched :: Five -> Bool
allScratched (Five (a, b, c, d, e)) = all (== -1) [a, b, c, d, e]

isWinner :: Board -> Bool
isWinner (Board b) = any allScratched b

processBoards :: Int -> [Board] -> [Board]
processBoards n boards = let newBoards = map (processBoard n) boards in
  filter (not . isWinner) newBoards

playOut :: Board -> [Int] -> (Board, Int)
playOut b [] = error "No inputs remain (playing out)"
playOut b (x:xs) = let newBoard = processBoard x b in
  if isWinner newBoard
    then (newBoard, x)
    else playOut newBoard xs

playBoards :: [Int] -> [Board] -> (Board, Int)
playBoards [] _ = error "No inputs remain"
playBoards (x:xs) boards = let newBoards = processBoards x boards in
  if length newBoards == 1 then playOut (head newBoards) xs else
  playBoards xs newBoards

sumFive :: Five -> Int
sumFive (Five (a, b, c, d, e)) = sum [x | x <- [a, b, c, d, e], x /= -1]

sumBoard :: Board -> Int
-- Take 5 = just the rows (otherwise we get double count)
sumBoard (Board b) = sum $ map sumFive (take 5 b) 

main :: IO ()
main = do
    raw <- getRawInput
    let (inputs, boards) = parseInput $ lines raw
    let (board, last) = playBoards inputs boards
    print $ sumBoard board
    print last
    print $ sumBoard board * last