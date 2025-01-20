module FsNodeOperations where

import FsNode (FsNode(..), getPath, fsName, isConsist)
import Utils (splitPath, removeLastElementInPath)
import Data.List (intercalate)

replace :: FsNode -> FsNode -> FsNode
replace (Directory path contents) file =
  Directory path (map (\x -> if getPath x == getPath file then file else x) contents)
replace _ _ = error "Not a directory"

updateDirectory :: FsNode -> FsNode -> FsNode -> FsNode
updateDirectory root p@(Directory path contents) file
  | path == getPath root = replace root file
  | isConsist contents file = updateDirectory root (getParent root p) (replace p file)
  | otherwise = updateDirectory root (getParent root p) (Directory path (contents ++ [file]))
updateDirectory _ (File _ _) _ = error "Not a directory"

mkEmptyDirectory :: FsNode -> FsNode -> String -> FsNode
mkEmptyDirectory root curr@(Directory path contents) name
 | path == getPath root = Directory path
 (contents ++ [Directory (path ++ name) []])
 | otherwise = updateDirectory root
 (getParent root curr) (Directory path (contents ++ [Directory (path ++ "/" ++ name) []]))
mkEmptyDirectory _ _ _ = error "You're not in a directory"

findChild :: String -> FsNode -> Maybe FsNode
findChild name (Directory _ contents) =
  foldr (\fs acc -> if fsName fs == name then Just fs else acc) Nothing contents
findChild _ _ = Nothing

removeFile :: FsNode -> FsNode -> FsNode -> FsNode
removeFile root curr@(Directory path contents) file
 | getPath root == path = Directory path (filter (/= file) contents)
 | otherwise = updateDirectory root (getParent root curr) (Directory (getPath curr)
 (filter (/= file) contents))
removeFile _ _ _ = error "Not a directory"

removeFiles :: FsNode -> FsNode -> [FsNode] -> FsNode
removeFiles root curr (f:files) = removeFiles (removeFile root (getParent root f) f)
 (cd root root (getPath curr)) files
removeFiles root _ [] = root

getFile :: FsNode -> FsNode -> String -> Maybe FsNode
getFile root curr path
  | null path = Just root
  | head path == '/' = findChild (last (splitPath path)) (cd root root 
  (removeLastElementInPath path))
  | null (removeLastElementInPath path) = findChild (last (splitPath path)) curr
  | otherwise = findChild (last (splitPath path)) (cd root curr (removeLastElementInPath path))

mkEmptyFile :: FsNode -> FsNode -> String -> FsNode
mkEmptyFile root curr@(Directory path contents) name
  | path == getPath root = Directory path (contents ++ [File (path ++ name) ""])
  | otherwise = updateDirectory root (getParent root curr)
 (Directory path (contents ++ [File (path ++ "/" ++ name) ""]))
mkEmptyFile _ _ _ = error "You're not in a directory"

mkFile :: FsNode -> FsNode -> String -> String -> FsNode
mkFile root curr@(Directory path contents) name content
  | path == getPath root = Directory path (contents ++ [File (path ++ name) content])
  | otherwise = updateDirectory root (getParent root curr)
 (Directory path (contents ++ [File (path ++ "/" ++ name) content]))
mkFile _ _ _ _ = error "You're not in a directory"

changeFileContent :: FsNode -> FsNode -> String -> FsNode
changeFileContent root f@(File path _) content = updateDirectory root (getParent root f)
 (File path content)
changeFileContent _ _ _ = error "Not a file"

catFiles :: [FsNode] -> String
catFiles files =
    intercalate "\n" $ concatMap (\f -> case f of
        File _ content -> [content]
        Directory _ _ -> []
    ) files

isFileExist :: FsNode -> FsNode -> String -> Bool
isFileExist root curr path = case getFile root curr path of
  Just (File _ _) -> True
  Just (Directory _ _) -> True
  Nothing -> False

getParentAbsPath :: FsNode -> String
getParentAbsPath path = removeLastElementInPath (getPath path)

getParent :: FsNode -> FsNode -> FsNode
getParent root file = if getPath parent == getPath root then root else parent
  where
    parent = cd root file (getParentAbsPath  file)

cd :: FsNode -> FsNode -> String -> FsNode
cd root current path
  | head path == '/' = cdRecursive root arrPath
  | otherwise = cdRecursive current arrPath
  where
    arrPath = splitPath path

    cdRecursive :: FsNode -> [String] -> FsNode
    cdRecursive fs [] = fs
    cdRecursive curr (dir:dirs)
     | dir == ".." = cdRecursive (getParent root curr) dirs
     | dir == "." = cdRecursive curr dirs
     | otherwise = case findChild dir curr of
          Just child -> cdRecursive child dirs
          Nothing -> curr

validatePath :: FsNode -> FsNode -> String -> Bool
validatePath root curr path
 | head path == '/' = validatePathRecursive root root arrayPath
 | otherwise = validatePathRecursive root curr arrayPath
 where
  arrayPath = splitPath path

  validatePathRecursive :: FsNode -> FsNode -> [String] -> Bool
  validatePathRecursive _ _ [] = True
  validatePathRecursive _ current@(Directory _ _) (dir:dirs)
    | dir == ".." = validatePathRecursive root (getParent root current) dirs
    | dir == "." = validatePathRecursive root current dirs
    | otherwise = case findChild dir current of
        Just child -> validatePathRecursive root child dirs
        Nothing -> False
  validatePathRecursive _ _ _ = False