module Day4 (someFunc) where

    import Data.List.Split (splitOn)
    import Data.Traversable

    someFunc :: IO ()
    someFunc = do
        contents <- lines <$> readFile "input4.txt"
        let numStrs = map (map (splitOn "-")) (map (splitOn ",") contents)
        let nums = map (map (map (read :: String -> Int))) numStrs
        let evaled = map evalRange2 nums
        print $ evaled
        print $ length $ filter (>= 0) evaled

    evalRange2 range = 
        if ((range!!0)!!0 - ((range!!1)!!1)) == 0 then 
            1
        else
            ((range!!1)!!0 - ((range!!0)!!1)) `div` ((range!!0)!!0 - ((range!!1)!!1))

    evalRange1 range = 
        if ((range!!0)!!1 - (range!!1)!!1) == 0 then
            -1
        else if ((range!!0)!!0 - (range!!1)!!0) == 0 then
            -1
        else 
            ((range!!0)!!0 - (range!!1)!!0) `div` ((range!!0)!!1 - (range!!1)!!1)