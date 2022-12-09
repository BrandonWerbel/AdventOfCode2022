module Day7 (someFunc) where

import Data.Tree

fileSystem :: Tree String
fileSystem = Node "/" [
        Node "a" [],
        Node "b" [
            Node "c" [],
            Node "d" []
            ]
        ]

folderD = Node "d" []


-- | This function takes a Tree and a String identifier and returns a list of all
-- | subtrees rooted at nodes with the given identifier.
findSubtrees :: Tree String -> String -> [Tree String]
findSubtrees tree id =
  case tree of
    -- If the root of the tree has the given identifier, return the whole tree.
    Node rootId _ | rootId == id -> [tree]
    -- If the root of the tree doesn't have the given identifier, search its
    -- children recursively.
    Node _ children ->
      -- The concatMap function takes a list and a function and applies the
      -- function to each element of the list, concatenating the resulting lists.
      concatMap (\child -> findSubtrees child id) children

findSubtree :: Tree String -> String -> Tree String
findSubtree tree id = head $ findSubtrees tree id

-- getParent :: Tree String -> Tree String -> Maybe (Tree String)
-- getParent parent child =
--   case parent of
--     -- If the parent node has the given child as one of its subtrees, return the
--     -- parent node.
--     Node _ children ->
--       if child `elem` children
--         then Just parent
--         else Nothing
--     -- Otherwise, search the parent's children recursively for the given child.
--     Node _ children ->
--       -- The find function takes a list and a function, and returns the first
--       -- element of the list that satisfies the given condition. Here, we're
--       -- looking for the first parent node that has the given child as one of its
--       -- subtrees.
--       find (\c -> getParent c child /= Nothing) children



someFunc :: IO ()
someFunc = do
    -- let newSystem = addNode folderD "b"
    -- putStrLn $ drawTree newSystem
    let newSystem = userInput fileSystem "$ cd b"
    putStrLn $ drawTree $ fst newSystem -- $ findSubtree fileSystem "b"


userInput :: Tree String -> String -> (Tree String, Tree String)
userInput currDir cmd =
    if take 4 cmd == "$ cd" then
        let newCurrDir = drop 5 cmd
        in (findSubtree fileSystem newCurrDir, fileSystem)
    else 
        (currDir, fileSystem)
