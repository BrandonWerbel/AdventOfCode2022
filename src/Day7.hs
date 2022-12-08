module Day7 (someFunc) where

    -- import Control.Monad
    -- import Data.List


    data FileStructure = Root [FileStructure] | File String Int | Directory FileStructure Char [FileStructure]
    -- File takes name and integer
    -- Directory take parent, name, contents
    root = Root [Directory (Root []) 'b' []]

    getName :: FileStructure -> String
    getName f = case f of
        File c _ -> c
        Directory _ c _ -> [c]
        Root _ -> "/"
        
    getSize :: FileStructure -> Int
    getSize f = case f of
        File _ n -> n
        Directory _ _ _ -> -1
        Root _ -> -1

    getFolderContents :: FileStructure -> [FileStructure]
    getFolderContents f = case f of
        File _ _ -> []
        Directory _ _ c -> c
        Root c -> c

    getParent :: FileStructure -> FileStructure
    getParent f = case f of
        File _ _ -> error "Cannot get parent of file"
        Directory p _ _ -> p
        Root _ -> f

    getInnerFolder :: FileStructure -> String -> FileStructure
    getInnerFolder f str = case f of
        File _ _ -> f
        Root dirs ->
            if length str == 1 then
                head $ filter (\x -> (getName x) == str) dirs
            else f
        Directory _ _ dirs -> 
           -- the name will always be a single char, unless the name is "..", in which case it should return the parent folder
            if length str == 1 then
                head $ filter (\x -> (getName x) == str) dirs
            else getParent f

    isRoot :: FileStructure -> Bool
    isRoot (Root _) = True
    isRoot _ = False



    setFolderContents :: FileStructure -> [FileStructure] -> FileStructure
    setFolderContents dir contents =
        if dir == root then
            Root contents
        else
            Directory (Root contents) 'd' contents

    updateParent file fileContents =
        let parent = getParent file
            parentName = head $ getName parent
            newParent = Directory parent parentName fileContents
        in newParent

    addFile :: FileStructure -> FileStructure -> FileStructure
    addFile dir newFile = setFolderContents dir (getFolderContents dir ++ [newFile])

    instance Show FileStructure where
        show (File c i) = "(" ++ c ++ ", " ++ (show i) ++ ")"
        show (Directory p c f) = "{" ++ [c] ++ ", " ++ (show f) ++ "} " ++ show p
        show (Root f) = show f

    instance Eq FileStructure where
        Root _ == Root _ =
            True
        File name1 size1 == File name2 size2 = 
            name1 == name2 && size1 == size2
        Directory parent1 name1 contents1 == Directory parent2 name2 contents2 =
             parent1 == parent2 && name1 == name2 && contents1 == contents2
        _ == _ = False


    someFunc :: IO ()
    someFunc = do
        -- let root = Directory root '/' []
        print root
        let root = Root []
        let root2 = addFile root (Directory root 'a' [])
        print root2
        let dirA = getInnerFolder root2 "a"
        -- let dirA2 = addFile dirA (File "x.txt" 12)
        -- print $ dirA2
        print $ getParent dirA


    -- updateDir :: String -> FileStructure -> FileStructure
    -- updateDir cmd currDir =
    --     if take 3 cmd == "dir" then
    --         addFile currDir (Directory currDir (cmd!!4) [])
    --     else if take 4 cmd == "$ cd" then
    --         getInnerFolder currDir $ drop 5 cmd
    --     else currDir
