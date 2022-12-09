module Test (someFunc) where


import Data.Tree
import Data.List
import Data.Maybe (fromMaybe)

fileSystem :: Tree String
fileSystem = Node "/" [
        Node "a" [],
        Node "b" [
            Node "c" [],
            Node "d" [
                Node "e" []
                ]
            ]
        ]

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


getParent :: Tree String -> Tree String -> Tree String
getParent parent child =
  case parent of
    -- If the parent node has the given child as one of its subtrees, return the
    -- parent node. Otherwise, search the parent's children recursively for the
    -- given child.
    Node _ children ->
      if child `elem` children
        then parent
        else fromMaybe parent (find (\c -> getParent c child /= parent) children)


someFunc :: IO ()
someFunc = do
    -- Find the first subtree rooted at a node with the identifier "d" in the
    -- fileSystem tree.
    let subtree = (head $ findSubtrees fileSystem "d")

    -- Get the parent node of the subtree.
    let parent = getParent fileSystem subtree

    -- Convert the tree to a string and print it.
    putStrLn $ drawTree parent
