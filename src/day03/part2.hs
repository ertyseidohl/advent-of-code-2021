module Main where

import Numeric (showIntAtBase, readInt)
import Data.Maybe (listToMaybe)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.IntMap.Strict (IntMap, fromList, adjust, (!))
import qualified Data.IntMap.Strict as IntMap

data BitCounter = BitCounter {zeros :: Int, ones :: Int}

getRawInput :: IO String
getRawInput = readFile "./src/day03/input"

-- From https://stackoverflow.com/a/5922212/374601
readBin :: String -> Maybe Int
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

getAnswer :: Maybe Int -> Maybe Int -> String
getAnswer (Just a) (Just b) = show $ a * b
getAnswer _ _ = error "Invalid gamma and epsilon values!"

incorporate :: IntMap Int -> (Int, Char) -> IntMap Int
incorporate btc (index, val) = if val == '1' then adjust (1 +) index btc else btc 

getOnesCount' :: IntMap Int -> String -> IntMap Int
getOnesCount' btc input = let enum = zip [0..] input in
  foldl' incorporate btc enum

getOnesCount :: [String] -> IntMap Int
getOnesCount input = let len = length $ head input in 
  let initialBitCounter = fromList $ zip [0..] $ replicate len 0 in
  foldl' getOnesCount' initialBitCounter input

getRating :: ([String] -> Int -> Char) -> [String] -> Int -> String
getRating _ [] _ = error "No  rating values left"
getRating _ [x] _ = x
getRating comp xs index = let searchBit = comp xs index in
  getRating comp (filter (\x -> (x !! index) == searchBit) xs) (index + 1)
    
getMostCommon :: [String] -> Int -> Char
getMostCommon xs index = if (getOnesCount xs ! index) * 2 >= length xs then '1' else '0'

getLeastCommon :: [String] -> Int -> Char
getLeastCommon xs index = if (getOnesCount xs ! index) * 2 >= length xs then '0' else '1'

computeAnswer :: Maybe Int -> Maybe Int -> Int
computeAnswer (Just a) (Just b) = a * b
computeAnswer _ _ = error "can't compute answer"

main :: IO ()
main = do
    raw <- getRawInput
    let rawLines = lines raw
    let oxygenRating = getRating getMostCommon rawLines 0
    let carbonRating = getRating getLeastCommon rawLines 0
    let o = readBin oxygenRating
    let c = readBin carbonRating
    print $ computeAnswer o c
    