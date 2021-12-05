module Main where

getRawInput :: IO String
getRawInput = readFile "./src/day01/input"

countSlidingIncrements :: [Int] -> Int
countSlidingIncrements [] = 0
countSlidingIncrements [_] = 0
countSlidingIncrements [_,_] = 0
countSlidingIncrements [_,_,_] = 0
countSlidingIncrements [w,x,y,z] = if w + x + y < x + y + z then 1 else 0
countSlidingIncrements xs@(w:x:y:z:_) = do
    let res = if w + x + y < x + y + z then 1 else 0
    res + countSlidingIncrements (tail xs)

main :: IO ()
main = do
    raw <- getRawInput
    let inc = countSlidingIncrements (map read $ lines raw)
    print inc
