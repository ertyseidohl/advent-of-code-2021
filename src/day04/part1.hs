module Main where

import Debug.Trace (trace)
import Data.List.Split ( splitOn )

data Cell = Cell {val :: Int, sel :: Bool} deriving (Show, Eq)

newtype Board = Board [[Cell]] deriving (Show)

getRawInput :: IO String
getRawInput = readFile "./src/day04/input"

splitNumbers :: String -> [Int]
splitNumbers xs = map read $ splitOn "," xs

processLine :: String -> [Cell]
processLine "" = []
processLine (' ':xs) = processLine xs
processLine xs = Cell (read (takeWhile (/= ' ') xs)) False : processLine (dropWhile (/= ' ') xs)

createBoard :: [String] -> Board
createBoard ("":xs) = createBoard xs
createBoard xs = Board $ map processLine xs

splitBoards :: [String] -> [Board]
splitBoards [] = []
splitBoards xs = createBoard (take 6 xs) : splitBoards (drop 6 xs)
 
parseInput :: [String] -> ([Int], [Board])
parseInput xs = (splitNumbers $ head xs , splitBoards $ tail xs)

at :: Board -> Int -> Int -> Cell
-- Why do I need the Board constuctor here?
at (Board board) x y = (board !! y) !! x

main :: IO ()
main = do
    raw <- getRawInput
    let (inputs, boards) = parseInput $ lines raw
    print (show inputs)
    print (show boards)