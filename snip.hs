-- | A snippet manager.
-- | Inspired by Learn You Some Haskell todo program.
module Snippet where

import Data.List
import System.Directory
import System.Environment
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

view [fileName] = do
    allSnips <- readFile fileName
    putStr allSnips

add (x:xs) = undefined
remove (x:xs) = undefined

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
    -- newTodo <- getLine
    -- withFile defaultPath AppendMode
    --     (`hPutStr` (newTodo ++ "\n"))

