-- | A snippet manager.
-- | Inspired by Learn You Some Haskell todo program.
module Snippet where

import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO

defaultFileName = "snip.txt"
defaultDirName = "."
defaultPath = defaultDirName ++ "/" ++ defaultFileName

-- Association list that connects command line options with functions.
dispatch :: [(String, [String] -> IO ())]
dispatch = [
    ("view", view)
  , ("add", add)
  , ("remove", remove)
           ]

view :: [FilePath] -> IO ()
view [] = return ()
view (x:xs) = do
    contents <- readFile x
    let
        allSnips = lines contents
        numberedSnips = zipWith
            (\n line -> show n ++ ": " ++ line) [0..] allSnips
    putStrLn $ map toUpper x
    putStr $ unlines numberedSnips
    view xs

add :: [String] -> IO ()
add [fileName, newSnip] =
    appendFile fileName (newSnip ++ "\n")

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tmpName, tmpHandle) <- openTempFile defaultDirName "tmp"
    contents <- hGetContents handle
    let
        (before, after) = splitAt (read numberString) $
            lines contents
        newAllSnips = unlines $ before ++ (drop 1 after)
    hPutStr tmpHandle newAllSnips
    removeFile fileName
    renameFile tmpName fileName

main = do
    (command:args) <- getArgs
    let maybeAction = lookup command dispatch
    -- Exit if command is unknown
    checkAction maybeAction command
    let Just action = maybeAction
    action args
        where
            checkAction Nothing command = do
                putStrLn $ "No command " ++ command
                exitWith $ ExitFailure 1
                return ()
            checkAction (Just _) _ = do
                return ()

