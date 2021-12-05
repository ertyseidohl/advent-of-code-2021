module Main where

import Debug.Trace
import Data.List.Split ( splitOn )
import Data.List (foldl')

data Dir = Up | Down | Forward deriving (Enum, Show, Eq)
data Inst = Inst Dir Int deriving (Show, Eq)

-- x, y, aim
data Pos = Pos {x :: Int, y :: Int, aim :: Int} deriving (Show, Eq)

getRawInput :: IO String
getRawInput = readFile "./src/day02/input"

toDir :: String -> Dir
toDir "up" = Up
toDir "down" = Down
toDir "forward" = Forward
toDir other = error $ "Invalid dir: " ++  other

driveSubmarine' :: Inst -> Pos -> Pos
driveSubmarine' (Inst Forward z) (Pos x y aim) = Pos (x + z) (y + (aim * z)) aim
driveSubmarine' (Inst Up z) (Pos x y aim) = Pos x y (aim - z)
driveSubmarine' (Inst Down z) (Pos x y aim) = Pos x y (aim + z)

driveSubmarine :: Pos -> Inst -> Pos
driveSubmarine p i = let new = driveSubmarine' i p in 
  trace (show new) new

parseInstruction :: String -> Inst
parseInstruction x = let d = splitOn " " x in
    Inst (toDir $ head d) (read $ d!!1)

main :: IO ()
main = do
    raw <- getRawInput
    let instructions = map parseInstruction $ lines raw
    let Pos x y z = foldl' driveSubmarine (Pos 0 0 0) instructions
    print $ show (x * y)
