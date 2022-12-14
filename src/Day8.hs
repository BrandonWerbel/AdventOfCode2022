module Day8 (someFunc) where

import Data.Char
import Data.List

checkSideVisibility' :: [Int] -> Int -> Bool
checkSideVisibility' arr index =
    let leftSide = take (index) arr
        rightSide = drop (index + 1) arr
        hiddenLeft = or (map (\n -> arr!!index <= n) leftSide)
        hiddenRight = or (map (\n -> arr!!index <= n) rightSide)
        canSee = not (hiddenLeft && hiddenRight)
    in canSee

checkSideVisibility :: [Int] -> [Bool]
checkSideVisibility line = map (checkSideVisibility' line) [0.. (length line - 1)]


someFunc :: IO ()
someFunc = do

    contents <- lines <$> readFile "input8.txt"
    let numsArray = map (map digitToInt) contents

    let rowsSee = map (\l -> checkSideVisibility l) numsArray
    let colsSee = transpose (map (\l -> checkSideVisibility l) (transpose numsArray))
    
    let canSee = zipWith (zipWith (||)) colsSee rowsSee
    let totalSeen = length (filter (\b -> b) (concat canSee))
    print totalSeen