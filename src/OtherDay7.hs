module OtherDay7 (someFunc) where

import Data.Tree
import Data.Char


-- | This function takes a Tree and a String identifier and returns a list of all
-- | subtrees rooted at nodes with the given identifier.
findSubtrees :: Tree (Int, String) -> (Int, String) -> [Tree (Int, String)]
findSubtrees tree id =
    case tree of
        -- If the root of the tree has the given identifier, return the whole tree.
        Node rootId _ | rootId == id -> [tree]
        Node _ children -> concatMap (\child -> findSubtrees child id) children
            

findSubtree :: Tree (Int, String) -> (Int, String) -> Tree (Int, String)
findSubtree tree id = head $ findSubtrees tree id

getParents :: Tree (Int, String) -> Tree (Int, String) -> [Tree (Int, String)]
getParents (Node id children) child =
    let parent = Node id children
    in if child `elem` children
        then [parent]
        else concatMap (\c -> getParents c child) children

getParent :: Tree (Int, String) -> Tree (Int, String) -> Tree (Int, String)
getParent parent child = head $ getParents parent child

subNode :: Tree (Int, String) -> Tree (Int, String) -> Tree (Int, String)
subNode (Node rootId children) (Node name newChildren) =
    let newNode = Node name newChildren
    in if rootId == name then newNode
        else Node rootId $ map (\c -> subNode c newNode) children

addNode :: Tree (Int, String) -> (Int, String) -> (Int, String) -> Tree (Int, String)
addNode tree location id =
    let innerDir = subForest (findSubtree tree location) ++ [Node id []]
        dir = Node location innerDir
        children = subForest tree
        newTree = subNode tree dir
    in newTree


    



printAscii :: Tree (Int, String) -> String
printAscii tree = printAscii' 0 tree

printAscii' :: Int -> Tree (Int, String) -> String
printAscii' indent (Node (size, name) []) = 
  "(" ++ show size ++ ", " ++ name ++ ")\n"
printAscii' indent (Node (size, name) children) = 
  "(" ++ show size ++ ", " ++ name ++ ")\n" ++
  (concat $ map (\child -> (replicate indent ' ') ++ "|\n" ++ (replicate indent ' ') ++ "|- " ++ printAscii' (indent+3) child) children)


userInput :: (Tree (Int, String), Tree (Int, String)) -> String -> (Tree (Int, String), Tree (Int, String))
userInput (fileSystem, currDir) cmd =
    if take 4 cmd == "$ cd" then
        let currDirId = (0, drop 5 cmd)
        in if currDirId == (0, "..") then (fileSystem, getParent fileSystem currDir)
            else (fileSystem, findSubtree fileSystem currDirId)
    else if take 3 cmd == "dir" then
            let newFileSystem = addNode fileSystem (rootLabel currDir) (0, drop 4 cmd)
                newCurrDir = findSubtree newFileSystem (rootLabel currDir)
            in (newFileSystem, newCurrDir)
    else if all isDigit (head $ words cmd) then
        let size = read ((words cmd)!!0) :: Int
            name = (words cmd)!!1
            newFileSystem = addNode fileSystem (rootLabel currDir) (size, name)
            newCurrDir = findSubtree fileSystem (rootLabel currDir)
        in (newFileSystem, newCurrDir)
    else (fileSystem, currDir)

userInputs :: (Tree (Int, String), Tree (Int, String)) -> [String] -> Int -> (Tree (Int, String), Tree (Int, String))
userInputs (fileSystem, currDir) cmds n =
    if n == (length cmds) - 1 then
        userInput (fileSystem, currDir) (cmds!!n)
    else
        userInputs (userInput (fileSystem, currDir) (cmds!!n)) cmds (n+1)

sumChildren :: Tree (Int, String) -> Tree (Int, String)
sumChildren (Node (value, name) children) =
  -- First, we sum up the children of the current node.
  let childSum = if length children == 0 then value else sum (map (\c -> fst (rootLabel c)) children)
  -- Then, we recursively sum the children of the current node's children.
  in Node (childSum, name) (map sumChildren children)


sumAllChildren :: Tree (Int, String) -> Tree (Int, String)
sumAllChildren tree =
    let summed = sumChildren tree
    in if summed == tree then summed else sumAllChildren summed

hasChildren :: Tree (Int, String) -> [(Int, String)]
hasChildren (Node val []) = []
hasChildren (Node (val, name) children) = filter (\(v, n) -> v <= 100000) $ (val, name) : (concatMap hasChildren children)


someFunc :: IO ()
someFunc = do
    contents <- lines <$> readFile "input7.txt"

    let fileSystem = Node (0, "/") []
    let currDir = fileSystem

    let (newFileSystem, newCurrDir) = userInputs (fileSystem, currDir) contents 0
    putStrLn $ printAscii newFileSystem

    let addedFileSystem = sumAllChildren newFileSystem
    putStrLn $ printAscii addedFileSystem

    let dirs = hasChildren addedFileSystem
    putStrLn $ show dirs

    let total = sum $ map fst dirs
    print total



    



