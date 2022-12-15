module Day8 (someFunc) where

import Data.Char
import Data.List
import Data.List.Split

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

countTreeVisibilityLeft :: [Int] -> Int -> Int
countTreeVisibilityLeft arr index = countTreeVisibilityLeft' arr index 1
    where countTreeVisibilityLeft' arr index n = 
            case (index - n) > 0 of
                False -> if index == 0 then 0 else 1
                True -> if arr!!(index - n) >= arr!!index then 1
                    else
                        1 + countTreeVisibilityLeft' arr index (n + 1)

countTreeVisibilityRight :: [Int] -> Int -> Int
countTreeVisibilityRight arr index = countTreeVisibilityRight' arr index 1
    where countTreeVisibilityRight' arr index n = 
            case (index + n) < length arr of
                False -> if index == length arr then 1 else 0
                True -> if arr!!(index + n) >= arr!!index then 1
                            else
                                1 + countTreeVisibilityRight' arr index (n + 1)

findTreeVisibilityScore :: (Int, Int) -> [[Int]] -> Int
findTreeVisibilityScore (x, y) arr =
    let rightScore = countTreeVisibilityRight (arr!!y) x
        leftScore = countTreeVisibilityLeft (arr!!y) x
        sidewaysArr = transpose arr
        upScore = countTreeVisibilityLeft (sidewaysArr!!x) y
        downScore = countTreeVisibilityRight (sidewaysArr!!x) y
    in rightScore * leftScore * upScore * downScore

someFunc :: IO ()
someFunc = do

    contents <- lines <$> readFile "input8.txt"
    let numsArray = map (map digitToInt) contents

    -- day 1
    -- let rowsSee = map (\l -> checkSideVisibility l) numsArray
    -- let colsSee = transpose (map (\l -> checkSideVisibility l) (transpose numsArray))
    
    -- let canSee = zipWith (zipWith (||)) colsSee rowsSee
    -- let totalSeen = length (filter (\b -> b) (concat canSee))
    -- print totalSeen

    -- day 2
    let indices = chunksOf (length (numsArray !! 0)) $ [(x, y) | x <- [0..length numsArray - 1], y <- [0..length (numsArray !! 0) - 1]]
    let results = map (map (\(x, y) -> findTreeVisibilityScore (y, x) numsArray)) indices
    let best = head $ reverse $ sort $ concat results


    mapM_ print results
    print best