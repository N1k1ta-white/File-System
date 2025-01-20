module FsNode where

import Utils (splitPath)

data FsNode
  = File String String
  | Directory String [FsNode]
  deriving (Eq)

instance Show FsNode where
  show f@(File _ _) = fsName f
  show d@(Directory _ _) = fsName d ++ "/"

fsName :: FsNode -> String
fsName (File path _) = last (splitPath path)
fsName (Directory path _) = last (splitPath path)

getPath :: FsNode -> String
getPath (File path _) = path
getPath (Directory path _) = path

isConsist :: [FsNode] -> FsNode -> Bool
isConsist (x:xs) file = (getPath x == getPath file &&
   isDirectory (Just x) == isDirectory (Just file)) || isConsist xs file
isConsist [] _ = False

isDirectory :: Maybe FsNode -> Bool
isDirectory (Just (Directory _ _)) = True
isDirectory _ = False