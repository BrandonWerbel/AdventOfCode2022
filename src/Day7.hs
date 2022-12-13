module Day7 (someFunc) where

import Data.Tree
import Data.List
import Data.Char

printAscii :: Tree (Int, String) -> String
printAscii tree = printAscii' 0 tree

printAscii' :: Int -> Tree (Int, String) -> String
printAscii' indent (Node (size, name) []) = 
  "(" ++ show size ++ ", " ++ name ++ ")\n"
printAscii' indent (Node (size, name) children) = 
  "(" ++ show size ++ ", " ++ name ++ ")\n" ++
  (concat $ map (\child -> (replicate indent ' ') ++ "|\n" ++ (replicate indent ' ') ++ "|- " ++ printAscii' (indent+3) child) children)


subAddedNode :: Tree (Int, String) -> Tree (Int, String) -> Tree (Int, String)
subAddedNode (Node rootId children) (Node name newChildren) =
    let newNode = Node name newChildren
    in if rootId == name && children == tail newChildren then newNode
        else Node rootId $ map (\c -> subAddedNode c newNode) children

updateParent :: ([Int], Tree (Int, String)) -> Tree (Int, String) -> ([Int], Tree (Int, String))
updateParent ([], newNode) tree = ([], newNode)
updateParent (p:[], newNode) tree = 
    let newTree = Node (rootLabel tree) (map (\c -> 
            if rootLabel c == rootLabel newNode then 
                newNode else c) (subForest tree))
    in ([], newTree)
updateParent ((p:path), newNode) tree =
    let newPath = take (length (p:path) - 1) (p:path)
        parent = getChild tree newPath
        newParent = Node (rootLabel parent) (map (\c -> 
            if rootLabel c == rootLabel newNode then 
                newNode else c) (subForest parent))
    in updateParent (newPath, newParent) tree

addNode :: Tree (Int, String) -> [Int] -> (Int, String) -> Tree (Int, String)
addNode tree path id =
    let newNode = Node id []
        parent = getChild tree path
        newParent = Node (rootLabel parent) (newNode : (subForest parent))
    in snd $ updateParent (path, newParent) tree

getChild :: Tree (Int, String) -> [Int] -> Tree (Int, String)
getChild tree [] = tree
getChild tree (p:path) = 
    let next = (subForest tree)!!p
    in getChild next path

findChildNum :: Tree (Int, String) -> String -> Int
findChildNum tree name =
    let zipped = zip [0..] (subForest tree)
        child = filter (\c -> snd (rootLabel (snd c)) == name) zipped
    in fst $ head child

sumAllChildren :: Tree (Int, String) -> Tree (Int, String)
sumAllChildren tree =
    let summed = sumChildren tree
    in if summed == tree then summed else sumAllChildren summed

sumChildren :: Tree (Int, String) -> Tree (Int, String)
sumChildren (Node (value, name) children) =
    let childSum = if length children == 0 then value else sum (map (\c -> fst (rootLabel c)) children)
    in Node (childSum, name) (map sumChildren children)

hasChildren1 :: Tree (Int, String) -> [(Int, String)]
hasChildren1 (Node val []) = []
hasChildren1 (Node (val, name) children) = filter (\(v, n) -> v <= 100000) $ (val, name) : (concatMap hasChildren1 children)

hasChildren2 :: Tree (Int, String) -> [(Int, String)]
hasChildren2 (Node val []) = []
hasChildren2 (Node (val, name) children) = (val, name) : (concatMap hasChildren2 children)


userInput :: (Tree (Int, String), [Int]) -> String -> (Tree (Int, String), [Int])
userInput (tree, path) cmd =
    if take 4 cmd == "$ cd" then
        let currDir = getChild tree path
            newCurrDirName = drop 5 cmd
        in if newCurrDirName == ".." then (tree, take (length path - 1) path) else 
            (tree, path ++ [findChildNum currDir newCurrDirName])
    else if take 3 cmd == "dir" then
        let newFileSystem = addNode tree path (0, drop 4 cmd)
        in (newFileSystem, path)
    else if all isDigit (head $ words cmd) then
        let size = read ((words cmd)!!0) :: Int
            name = (words cmd)!!1
            newFileSystem = addNode tree path (size, name)
        in (newFileSystem, path)
    else (tree, path)

userInputs :: (Tree (Int, String), [Int]) -> [String] -> Int -> (Tree (Int, String), [Int])
userInputs (fileSystem, path) cmds n =
    if n == (length cmds) - 1 then
        userInput (fileSystem, path) (cmds!!n)
    else
        userInputs (userInput (fileSystem, path) (cmds!!n)) cmds (n+1)


someFunc :: IO ()
someFunc = do
    contents <- lines <$> readFile "input7.txt"
    let useContents = tail contents

    let fileSystem = Node (0, "/") []
    let path = []

    let (newFileSystem, newPath) = userInputs (fileSystem, path) useContents 0
    let addedFileSystem = sumAllChildren newFileSystem
    putStrLn $ printAscii addedFileSystem

    -- let dirs = hasChildren1 addedFileSystem
    -- let total = sum $ map fst dirs

    -- part 2
    let total = sum $ map fst (flatten newFileSystem)
    let unused = 70000000 - total
    let needToDelete = 30000000 - unused 
    
    let filteredSortedDirs = filter (\d -> fst d >= needToDelete) (sortOn fst $ hasChildren2 addedFileSystem)

    print $ head filteredSortedDirs
