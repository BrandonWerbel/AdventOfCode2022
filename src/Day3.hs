module Day3 (someFunc) where


-- Written by ChatGPT, because I was curious
import Data.Maybe (mapMaybe)
import Data.Char (isLetter, isUpper)
import Data.List (find, intersect)

splitAndFindFirstLetter :: [String] -> Maybe Int
splitAndFindFirstLetter ss = do
    let n = length ss
    let firstThird = unwords (take (ceiling (fromIntegral n / 3)) ss)
    let secondThird = unwords (take (ceiling (fromIntegral n / 3)) (drop (ceiling (fromIntegral n / 3)) ss))
    let thirdThird = unwords (drop (ceiling (fromIntegral (n * 2) / 3)) ss)
    let maybeLetter = find (\c -> c `elem` firstThird && c `elem` secondThird) thirdThird
    case maybeLetter of
      Just letter -> do
        let number = fromEnum letter - (if isUpper letter then 38 else 96)
        return number
      Nothing -> Nothing

someFunc :: IO ()
someFunc = do
    input <- lines <$> readFile "input3.txt"
    let total = sum (mapMaybe splitAndFindFirstLetter (chunksOf3 input))
    print total

chunksOf3 :: [a] -> [[a]]
chunksOf3 [] = []
chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)
