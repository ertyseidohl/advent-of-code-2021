module Main where

newtype MyType = MyType [Int] deriving (Show)

demo2 :: Int -> Int -> Int
demo2 a b = a * b

demo :: Int -> MyType -> MyType
demo n (MyType xs) = MyType $ map (demo2 n) xs

main :: IO ()
main = do
  print $ demo 2 (MyType [2, 3, 4])