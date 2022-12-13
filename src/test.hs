module Test (someFunc) where


import Data.Tree
import Data.List

printAscii :: Tree (Int, String) -> String
printAscii tree = printAscii' 0 tree

printAscii' :: Int -> Tree (Int, String) -> String
printAscii' indent (Node (size, name) []) = 
  "(" ++ show size ++ ", " ++ name ++ ")\n"
printAscii' indent (Node (size, name) children) = 
  "(" ++ show size ++ ", " ++ name ++ ")\n" ++
  (concat $ map (\child -> (replicate indent ' ') ++ "|\n" ++ (replicate indent ' ') ++ "|- " ++ printAscii' (indent+3) child) children)


getChild :: Tree a -> [Int] -> Tree a
getChild tree [] = tree
getChild tree (p:path) = 
    let next = (subForest tree)!!p
    in getChild next path

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


someFunc :: IO ()
someFunc = do
    let tree = Node (0, "1") [
            Node (0, "2") [],
            Node (0, "3") [
                Node (0, "2") []]]
    let path = []
    let id = (1, "4")

    -- let parent = getChild tree path
    -- let newParent = Node (rootLabel parent) ( child : (subForest parent))
    -- let (newPath, newParentParent) = updateParent (path, newParent) tree

    let newTree = addNode tree path id
    
    putStrLn $ printAscii newTree