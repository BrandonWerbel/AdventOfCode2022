module Day5 (someFunc) where

import Data.List
import Data.Char

someFunc :: IO ()
someFunc = do
    content <- lines <$> readFile "input5.txt"
    let (stacksMap, instructions) = break (\x -> (x !! 1) == '1') content
    let cols = transpose $ stacksMap
    let zipped = zip [1..] cols
    let filtered = filter (\(i, s) -> i `mod` 4 == 2) zipped
    let stacks = map (filter (\x -> x /= ' ')) (map (\(_, s) -> s) filtered)
    print stacks

    let moves = map (filter isDigit) $ drop 2 instructions

    let movesInts = map (\m -> [read (take (length m - 2) m)] ++ map (\x -> digitToInt x - 1) (drop (length m - 2) m)) moves
    print movesInts
    print $ rearrange stacks movesInts


-- This function applies a single rearrangement instruction to the given list of stacks.
-- applyInstruction :: [[Char]] -> [Int] -> [[Char]]
applyInstruction stacks [n, from, to] =
  let
      (removed, updatedFrom) = splitAt n (stacks !! from)
      -- Append the removed characters to the stack at index `to`
      updatedTo = (reverse removed) ++ (stacks!!to)
      zipped = zip [0..] stacks
      newStacks = map (\(i, x) ->
            if i == from then updatedFrom
            else if i == to then updatedTo
            else x) zipped
  in -- Return the updated list of stacks, with the modified `from` and `to` stacks
     newStacks

-- This function applies a list of rearrangement instructions to the given list of stacks.
applyInstructions :: [[Char]] -> [[Int]] -> [[Char]]
applyInstructions stacks [] = stacks
applyInstructions stacks (inst:insts) =
    applyInstructions (applyInstruction stacks inst) insts

-- This is the main function which takes a list of strings and a list of instructions
-- and returns the first character of each string in the final list of stacks.
rearrange :: [[Char]] -> [[Int]] -> [Char]
rearrange stacks instructions =
  -- Apply the instructions to the list of stacks
  let updatedStacks = applyInstructions stacks instructions
  in map (head) updatedStacks

