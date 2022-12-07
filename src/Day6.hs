module Day6 (someFunc) where

import Data.List

chars = 14

someFunc :: IO ()
someFunc = do
    contents <- readFile "input6.txt"
    let subStrings = map nub [take chars (drop i contents) | i <- [0..length contents - chars]]
    let zipped = (zip [1..] subStrings)
    let filtered = filter (\(i, s) -> length s == chars) zipped 
    print $ fst (head filtered) + chars - 1
