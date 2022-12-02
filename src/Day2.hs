module Day2 (someFunc) where

someFunc :: IO ()
someFunc = do
    contents <- readFile "input2.txt"
    let list = map calcVal2 (map (map toInt) (map words (lines contents)))
    print (addVals list 0)

toInt val =
    if val == "A" || val == "X" then
        0
    else if val == "B" || val == "Y" then
        1
    else if val == "C" || val == "Z" then
        2
    else error "Wrong value"

calcVal2 moves =
    if moves!!1 == 1 then
        3 + moves!!0 + 1
    else  if moves!!1 == 0 then
        0 + (mod (moves!!0 - 1) 3) + 1
    else if moves!!1 == 2 then
        6 + (mod (moves!!0 + 1) 3) + 1
    else error $ show moves
    

calcVal1 moves =
    if moves!!0 == moves!!1 then
        3 + moves!!1
    else if mod (moves!!0 + 1) 3 == mod (moves!!1) 3 then
        6 + moves!!1
    else if mod (moves!!0) 3 == mod (moves!!1 + 1) 3 then
        0 + moves!!1
    else error $ show moves

addVals vals i =
    if i < length vals then
        vals!!i + addVals vals (i+1)
    else 0