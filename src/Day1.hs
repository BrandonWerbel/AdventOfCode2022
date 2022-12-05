module Day1 (someFunc, toInt) where

import Data.List

someFunc :: IO ()
someFunc = do
    contents <- readFile "input.txt"
    let listNums = (map toInt (lines contents)) ++ [0]
    let val = (addFunc listNums 0 0 [])
    let cals = sort val
    let total = (cals!!(length cals - 1)) + (cals!!(length cals - 2)) + (cals!!(length cals - 3))
    print total
        

toInt str = 
    if str == ""
        then 0
    else
        read str :: Integer

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

addFunc list val i vals =
    if list!!i == 0 then
        if length list < 17 then
            vals
        else
            addFunc (slice (i+1) (length list) list) 0 0 (vals++[val])
    else
        addFunc list (val + list!!i) (i+1) vals