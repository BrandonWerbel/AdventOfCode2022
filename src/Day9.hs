module Day9 (someFunc) where

import Data.Char
import Data.List

isTouching :: ((Int, Int), (Int, Int)) -> Bool
isTouching ((hX, hY), (tX, tY)) =
    let dx = hX - tX
        dy = hY - tY
    in (dx^2 + dy^2) <= 2

moveHeadOne :: ((Int, Int), (Int, Int)) -> Char -> ((Int, Int), (Int, Int))
moveHeadOne ((hX, hY), (tX, tY)) 'U' =
    if isTouching ((hX, hY+1), (tX, tY)) then ((hX, hY+1), (tX, tY))
        else ((hX, hY+1), (hX, hY))
moveHeadOne ((hX, hY), (tX, tY)) 'D' =
    if isTouching ((hX, hY-1), (tX, tY)) then ((hX, hY-1), (tX, tY))
        else ((hX, hY-1), (hX, hY))
moveHeadOne ((hX, hY), (tX, tY)) 'L' =
    if isTouching ((hX-1, hY), (tX, tY)) then ((hX-1, hY), (tX, tY))
        else ((hX-1, hY), (hX, hY))
moveHeadOne ((hX, hY), (tX, tY)) 'R' =
    if isTouching ((hX+1, hY), (tX, tY)) then ((hX+1, hY), (tX, tY))
        else ((hX+1, hY), (hX, hY))
moveHeadOne pos dir = error (show pos)

moveHead :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ([(Int, Int)], (Int, Int))
moveHead old (d, i) =
    (moveHead' old (d, i) [])
    where moveHead' old (d, i) currList = 
            let next = moveHeadOne old d
            in case i == 0 of
                True -> (reverse currList, snd next)
                False -> moveHead' (next) (d, i-1) ((snd next):currList)


followDirections :: ((Int, Int), (Int, Int)) -> [(Char, Int)] -> [(Int, Int)]
followDirections starting directions =
    followDirections' starting directions []

followDirections' :: ((Int, Int), (Int, Int)) -> [(Char, Int)] -> [(Int, Int)] -> [(Int, Int)]
followDirections' starting [] currList = currList
followDirections' starting (d:directions) currList =
    let (next, hPos) = moveHead starting d
    in followDirections' (hPos, next!!(length next - 1)) directions (currList ++ next)

        

someFunc :: IO ()
someFunc = do

    contents <- lines <$> readFile "input9.txt"
    let tuplesContent = map (\l -> (l!!0, digitToInt (l!!2))) contents

    let (hX, hY) = (0, 0)
    let (tX, tY) = (0, 0)

    -- let (hX, hY) = (4, 1)
    -- let (tX, tY) = (3, 0)

    let startingPos = ((hX, hY), (tX, tY))


    -- mapM_ print tuplesContent

    -- print $ fst $ moveHead startingPos ('U', 4)

    let path = followDirections startingPos tuplesContent

    print $ length $ nub path