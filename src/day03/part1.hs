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

getGamma :: IntMap Int -> Int -> Int -> String
getGamma btc count size = let half = div count 2 in
  map (\z -> if (btc ! z) > half then '1' else '0') [0 .. size - 1]

invertBits :: String -> String
invertBits cs = [if c == '0' then '1' else '0' | c <- cs]

main :: IO ()
main = do
    raw <- getRawInput
    let rawLines = lines raw
    let count = length rawLines
    let size = length $ head rawLines
    let onesCount = getOnesCount rawLines
    let gamma = getGamma onesCount count size
    let epsilon = invertBits gamma
    print $ gamma ++ " " ++ epsilon
    print $ getAnswer (readBin gamma) (readBin epsilon)
