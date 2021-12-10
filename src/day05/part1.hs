module Main where

import Data.Coerce (coerce)
import Data.List (foldl')
import Data.List.Split ( splitOn )
import Data.Map.Strict (Map, insertWith, empty)
import qualified Data.Map.Strict (filter)

data Line = Line {start :: Point, end :: Point} deriving (Show)

type Point = (Int, Int)
type Ocean = Map Point Int

getRawInput :: IO String
getRawInput = readFile "./src/day05/input"

paintLine :: Ocean -> Line -> Ocean
paintLine o l = foldl' incr o (allPoints l)

incr :: Ocean -> Point -> Ocean
incr o p = insertWith (+) p 1 o

allPoints :: Line -> [Point]
allPoints l = allPoints' (start l) (end l)

allPoints' :: Point -> Point -> [Point]
allPoints' a b
  | a == b = [a]
  | otherwise = a : allPoints' (nextPoint a b) b

isAxisAligned :: Line -> Bool
isAxisAligned (Line (ax, ay) (bx, by)) = ax == bx || ay == by

nextPoint :: Point -> Point -> Point
nextPoint (ax, ay) (bx, by)
  | ax == bx && ay == by = error $ "Same point can't be next point: " ++ show ax ++ "," ++ show ay
  | ax == bx && ay < by = (ax, ay + 1)
  | ax == bx && ay > by = (ax, ay - 1)
  | ax > bx && ay == by = (ax - 1, ay)
  | ax < bx && ay == by = (ax + 1, ay)
  | otherwise = error $ "Non-axis-aligned lines?? " ++ show ax ++ "," ++ show ay ++ " / " ++ show bx ++ "," ++ show by

pointFromList :: [Int] -> Point
pointFromList [a, b] = (a, b)
pointFromList _ = error "illegal point list!"

createLine :: String -> Line
createLine xs = Line start end where
  points = splitOn " -> " xs
  start = pointFromList $ map read $ splitOn "," $ head points
  end = pointFromList $ map read $ splitOn "," $ points !! 1

createLines :: [String] -> [Line]
createLines x = filter isAxisAligned $ map createLine x

countOverlaps :: Ocean -> Int
countOverlaps o = length $ Data.Map.Strict.filter (> 1) o

main :: IO ()
main = do
  raw <- getRawInput
  let allLines = createLines $ lines raw
  let ocean = foldl' paintLine empty allLines
  print $ countOverlaps ocean
