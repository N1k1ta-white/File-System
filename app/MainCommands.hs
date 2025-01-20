module MainCommands where

import Data.Maybe (catMaybes)

import FsNode (FsNode(..), getPath, isDirectory)
import Utils (removeLastElementInPath)
import FsNodeOperations (removeFiles, getFile, changeFileContent, mkFile)
import Control.Monad (when)

pwd :: FsNode -> String
pwd = getPath

ls :: FsNode -> [String]
ls (Directory _ contents) = map show contents
ls _ = ["Not a directory"]

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