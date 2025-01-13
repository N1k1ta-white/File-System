{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Main(module Main) where

import System.IO (hFlush, stdout)
import GHC.IO.Exception()
import System.Exit (exitSuccess)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad (when, unless)
import Data.List (intercalate)

data FsNode
  = File String String
  | Directory String [FsNode]
  deriving (Eq)

instance Show FsNode where
  show f@(File _ _) = fsName f
  show d@(Directory _ _) = fsName d ++ "/"

isConsist :: [FsNode] -> FsNode -> Bool
isConsist (x:xs) file = (getPath x == getPath file &&
   isDirectory (Just x) == isDirectory (Just file)) || isConsist xs file
isConsist [] _ = False

replace :: FsNode -> FsNode -> FsNode
replace (Directory path contents) file =
  Directory path (map (\x -> if getPath x == getPath file then file else x) contents)
replace _ _ = error "Given argument is not a directory"

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

getPath :: FsNode -> String
getPath (File path _) = path
getPath (Directory path _) = path

pwd :: FsNode -> String
pwd = getPath

ls :: FsNode -> [String]
ls (Directory _ contents) = map show contents
ls _ = ["Not a directory"]

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

splitPath :: String -> [String]
splitPath = filter (/= ".") . words . map (\c -> if c == '/' then ' ' else c)

removeLastElementInPath :: String -> String
removeLastElementInPath path = reverse (dropWhile (/= '/') (reverse path))

getParentAbsPath :: FsNode -> String
getParentAbsPath path = removeLastElementInPath (getPath path)

getParent :: FsNode -> FsNode -> FsNode
getParent root file = if getPath parent == getPath root then root else parent
  where
    parent = cd root file (getParentAbsPath  file)

findChild :: String -> FsNode -> Maybe FsNode
findChild name (Directory _ contents) =
  foldr (\fs acc -> if fsName fs == name then Just fs else acc) Nothing contents
findChild _ _ = Nothing

fsName :: FsNode -> String
fsName (File path _) = last (splitPath path)
fsName (Directory path _) = last (splitPath path)

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

changeFileContent :: FsNode -> FsNode -> String -> FsNode
changeFileContent root f@(File path _) content = updateDirectory root (getParent root f)
 (File path content)
changeFileContent _ _ _ = error "Not a file"

getFile :: FsNode -> FsNode -> String -> Maybe FsNode
getFile root curr path
  | null path = Just root
  | head path == '/' = findChild (last (splitPath path)) (cd root root 
  (removeLastElementInPath path))
  | null (removeLastElementInPath path) = findChild (last (splitPath path)) curr
  | otherwise = findChild (last (splitPath path)) (cd root curr (removeLastElementInPath path))

isFileExist :: FsNode -> FsNode -> String -> Bool
isFileExist root curr path = case getFile root curr path of
  Just (File _ _) -> True
  Just (Directory _ _) -> True
  Nothing -> False

-- Използвана модел: Copilot
-- Запитване: write code to check for redirection (">"), split arguments into input 
--            files and an optional output file, and concatenate content from multiple files
-- Оригинален отговор: не запазил съм го :(
-- Направени промени: в функция catFiles смених [String] -> String на [FsNode] -> String
hasRedirection :: [String] -> Bool
hasRedirection = elem ">"

splitArgs :: [String] -> ([String], Maybe String)
splitArgs args = case break (== ">") args of
    (inputs, ">":output:_) -> (inputs, Just output)
    (inputs, _) -> (inputs, Nothing)

catFiles :: [FsNode] -> String
catFiles files =
    intercalate "\n" $ concatMap (\f -> case f of
        File _ content -> [content]
        Directory _ _ -> []
    ) files
-- завърша генериран код от Copilot

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

isDirectory :: Maybe FsNode -> Bool
isDirectory (Just (Directory _ _)) = True
isDirectory _ = False

-- Използвана модел: Copilot
-- Запитване: make function to read lines until a dot (".") 
--            and return them as a single string
-- Оригинален отговор: същ
-- Направени промени: няма
readUntilDot :: IO String
readUntilDot = do
    line <- getLine
    if line == "."
        then return ""
        else do
            rest <- readUntilDot
            return $ if null rest
                    then line
                    else line ++ "\n" ++ rest
-- завърша генериран код от Copilot

-- REPL loop
repl :: FsNode -> FsNode -> IO ()
repl root state = do
  putStr (pwd state ++ " > ")
  hFlush stdout
  command <- getLine
  newState <- execute root command state
  case words command of
    ["mkdir", _] -> repl newState (cd newState newState (getPath state))
    ["touch", _] -> repl newState (cd newState newState (getPath state))
    ("rm":_) -> repl newState (cd newState newState (getPath state))
    ("cat":_) -> repl newState (cd newState newState (getPath state))
    _ -> repl root newState

catWritingToFile :: FsNode -> FsNode -> String -> String -> IO FsNode
catWritingToFile root state file text =
  case getFile root state file of
      Just f -> return (changeFileContent root f text)
      Nothing -> do
        let parentPath = removeLastElementInPath file
        if null parentPath then
          return (mkFile root state file text)
        else do
          let parentDir = getFile root state (init parentPath) -- init removes last '/' symmbol
          case parentDir of
            Just p@(Directory _ _) -> return (mkFile root p file text)
            Just (File _ _) -> do
              putStrLn "cat: Invalid path: parent is a file"
              return root
            Nothing -> do
              putStrLn ("cat: Invalid path: parent directory does not exist " ++ parentPath)
              return root

rm :: FsNode -> FsNode -> [String] -> IO FsNode
rm root state args = do
  if head args == "-r" then do
      let entry = map (getFile root state) (tail args)
      let files = catMaybes entry
      when (length entry /= length files) $ do
        putStrLn "rm: invalid path"
      return (removeFiles root state files)
    else do
      let entry = map (getFile root state) args
      let files = filter (not . isDirectory) (map (getFile root state) args)
      if length entry == length files then do
        when (length files /= length (catMaybes entry)) $ do
          putStrLn "rm: invalid path"
        return (removeFiles root state (catMaybes entry))
      else do
        putStrLn "rm: cannot remove directory. Use 'rm -r' for directories."
        return (removeFiles root state (catMaybes entry))

execute :: FsNode -> String -> FsNode -> IO FsNode
execute root command state = case words command of
  ["pwd"] -> do
    putStrLn (pwd state)
    return state
  ["ls"] -> do
    mapM_ putStrLn (ls state)
    return state
  ["ls", path] -> do
    mapM_ putStrLn (ls (cd root state path))
    return state
  ["cd", dir] -> do
    if validatePath root state dir
      then return (cd root state dir)
      else do
        putStrLn "cd: Invalid path"
        return state
  ["mkdir", dir] -> do
    if isFileExist root state dir then do
      putStrLn "mkdir: File with this name already exists"
      return root
    else do
      let parentPath = removeLastElementInPath dir
      if null parentPath then
        return (mkEmptyDirectory root state dir)
      else do
        let parentDir = getFile root state (init parentPath)
        case parentDir of
          Just p@(Directory _ _) -> return (mkEmptyDirectory root p dir)
          Just (File _ _) -> do
            putStrLn "mkdir: Invalid path: parent is a file"
            return root
          Nothing -> do
            putStrLn ("mkdir: Invalid path: parent directory does not exist " ++ parentPath)
            return root
  ["touch", file] -> do
    if isFileExist root state file then do
      putStrLn "touch: File with this name already exists"
      return root
    else do
      let parentPath = removeLastElementInPath file
      if null parentPath then
        return (mkEmptyFile root state file)
      else do
        let parentDir = getFile root state (init parentPath)
        case parentDir of
          Just p@(Directory _ _) -> return (mkEmptyFile root p file)
          Just (File _ _) -> do
            putStrLn "touch: Invalid path: parent is a file"
            return root
          Nothing -> do
            putStrLn ("touch: Invalid path: parent directory does not exist " ++ parentPath)
            return root
  ["cat", ">", file] -> do
    hFlush stdout
    text <- readUntilDot
    catWritingToFile root state file text
  ("cat":args) -> do
        let (inputFiles, mbOutput) = splitArgs args
        let files = filter (not . isDirectory . Just) $ mapMaybe (getFile root state) inputFiles
        when (length inputFiles /= length files) $ do
          putStrLn "cat: invalid path"
        let content = catFiles files
        case mbOutput of
            Just outFile ->
                catWritingToFile root state outFile content
            Nothing -> do
                putStr content
                unless (null content) $ putStrLn ""
                return root
  ("rm":args) -> do
    rm root state args
  ["exit"] -> do
    putStrLn "Goodbye!"
    hFlush stdout
    exitSuccess
  _ -> do
    putStrLn "Invalid command"
    return state

initFS :: IO ()
initFS = do
  let root = Directory "/" [Directory "/home" [Directory "/home/test" []],
       Directory "/user" [File "/user/file.txt" "One file"]]
  repl root root

main :: IO ()
main = do
  putStrLn "Welcome to the Haskell File System!"
  initFS