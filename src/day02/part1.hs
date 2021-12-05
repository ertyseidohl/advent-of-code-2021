module Main where

import Data.List.Split ( splitOn )

data Dir = Up | Down | Forward deriving (Enum, Show, Eq)
data Inst = Inst Dir Int deriving (Show, Eq)
data Pos = Pos Int Int deriving (Show, Eq)

getRawInput :: IO String
getRawInput = readFile "./src/day02/input"

toDir :: String -> Dir
toDir "up" = Up
toDir "down" = Down
toDir "forward" = Forward
toDir other = error $ "Invalid dir: " ++  other

driveSubmarine :: Inst -> Pos -> Pos
driveSubmarine (Inst Forward z) (Pos x y) = Pos (x + z) y
driveSubmarine (Inst Up z) (Pos x y) = Pos x (y - z)
driveSubmarine (Inst Down z) (Pos x y) = Pos x (y + z)

parseInstruction :: String -> Inst
parseInstruction x = let d = splitOn " " x in
    Inst (toDir $ head d) (read $ d!!1)

main :: IO ()
main = do
    raw <- getRawInput
    let instructions = map parseInstruction $ lines raw
    let Pos x y = foldr driveSubmarine (Pos 0 0) instructions
    print $ show (x * y)
