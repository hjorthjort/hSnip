-- | A snippet manager.
-- | Inspired by Learn You Some Haskell todo program.
module Snippet where

import Flags

import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO

defaultFileName = "snip.txt"
defaultDirName = "."
defaultPath = defaultDirName ++ "/" ++ defaultFileName

view :: FilePath -> IO ()
view f = do
    contents <- readFile f
    let
        allSnips = lines contents
        numberedSnips = zipWith
            (\n line -> show n ++ ": " ++ line) [0..] allSnips
    putStrLn $ map toUpper f
    putStr $ unlines numberedSnips

add :: FilePath -> String -> IO ()
add fileName newSnip =
    appendFile fileName (newSnip ++ "\n")

remove :: FilePath -> Int -> IO ()
remove fileName n = do
    handle <- openFile fileName ReadMode
    (tmpName, tmpHandle) <- openTempFile defaultDirName "tmp"
    contents <- hGetContents handle
    let
        (before, after) = splitAt n $
            lines contents
        newAllSnips = unlines $ before ++ (drop 1 after)
    hPutStr tmpHandle newAllSnips
    removeFile fileName
    renameFile tmpName fileName

main = do
    args <- getArgs
    let flags = getFlags args
        fileName = if isSet "f" flags
                      then getFlagArg "f" flags
                      else defaultFileName
    if isSet "v" flags
       then view fileName
       else return ()
    if isSet "a" flags
       then add fileName $ getFlagArg "a" flags
       else return ()
    if isSet "r" flags
       then remove fileName $ read $ getFlagArg "r" flags
       else return ()
