module Utils where

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


splitArgs :: [String] -> ([String], Maybe String)
splitArgs args = case break (== ">") args of
    (inputs, ">":output:_) -> (inputs, Just output)
    (inputs, _) -> (inputs, Nothing)

removeLastElementInPath :: String -> String
removeLastElementInPath path = reverse (dropWhile (/= '/') (reverse path))

splitPath :: String -> [String]
splitPath = filter (/= ".") . words . map (\c -> if c == '/' then ' ' else c)

hasRedirection :: [String] -> Bool
hasRedirection = elem ">"