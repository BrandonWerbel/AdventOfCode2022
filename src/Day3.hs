module Day3 (someFunc) where


-- Written by ChatGPT, because I was curious
import Data.Maybe (mapMaybe)
import Data.Char (isLetter, isUpper)
import Data.List (find, intersect)

splitAndFindFirstLetter :: String -> Maybe Int
splitAndFindFirstLetter s = do
    let n = length s
    let (firstHalf, secondHalf) = splitAt (ceiling (fromIntegral n / 2)) s
    let matchingLetters = intersect firstHalf secondHalf
    let letter = head matchingLetters
    let number = fromEnum letter - (if isUpper letter then 38 else 96)
    return number

someFunc :: IO ()
someFunc = do
    input <- lines <$> readFile "input3.txt"
    let total = sum (mapMaybe splitAndFindFirstLetter input)
    print total