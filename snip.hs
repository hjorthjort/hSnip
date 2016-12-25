-- | A snippet manager.
-- | Inspired by Learn You Some Haskell todo program.

import Flags

import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO

-- We probably want to create this dynamically later, so we use a monad.
defaultFile :: IO FilePath
defaultFile = do
    return "snip.txt"

defaultDir :: IO FilePath
defaultDir = do
    snipDir <- (++ "/.snip") `fmap` getHomeDirectory
    createDirectoryIfMissing False snipDir
    return snipDir

defaultPath :: IO FilePath
defaultPath = do
    dir <- defaultDir
    file <- defaultFile
    return $ dir ++ "/" ++ file

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
    tmpDir <- defaultDir
    handle <- openFile fileName ReadMode
    (tmpName, tmpHandle) <- openTempFile tmpDir "tmp"
    contents <- hGetContents handle
    let
        (before, after) = splitAt n $
            lines contents
        newAllSnips = unlines $ before ++ (drop 1 after)
    hPutStr tmpHandle newAllSnips
    hClose handle
    hClose tmpHandle
    removeFile fileName
    renameFile tmpName fileName

main = do
    args <- getArgs
    let flags = getFlags args
    fileName <- if isSet "f" flags
                   then return $ getFlagArg "f" flags
                   else defaultPath
    if isSet "v" flags
       then view fileName
       else return ()
    if isSet "a" flags
       then add fileName $ getFlagArg "a" flags
       else return ()
    if isSet "r" flags
       then remove fileName $ read $ getFlagArg "r" flags
       else return ()
