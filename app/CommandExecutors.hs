module CommandExecutors where
import FsNode (FsNode (..), getPath, isDirectory)
import FsNodeOperations (mkEmptyDirectory, mkEmptyFile, getFile, cd, isFileExist, catFiles, validatePath)
import MainCommands (pwd, ls, rm, catWritingToFile)

import System.IO (hFlush, stdout)
import Utils (removeLastElementInPath, readUntilDot, splitArgs)
import Control.Monad (when, unless)
import Data.Maybe (mapMaybe)
import System.Exit (exitSuccess)

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
