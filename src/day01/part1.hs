module Main where

getRawInput :: IO String
getRawInput = readFile "./src/day01/input"

countIncrements :: [String] -> Int
countIncrements [] = 0
countIncrements [_] = 0
countIncrements (a:rest)
    | a <= head rest = 1 + countIncrements rest
    | otherwise = 0 + countIncrements rest

main :: IO ()
main = do
    raw <- getRawInput
    let inc = countIncrements (lines raw)
    print (inc + 1) -- lol
