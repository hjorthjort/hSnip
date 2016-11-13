module Snippet where

import System.IO (withFile, hPutStr, IOMode(AppendMode))

defaultFileName = "snip.txt"
defaultDirName = "."

main = do
    newTodo <- getLine
    withFile (defaultDirName ++ "/" ++ defaultFileName) AppendMode
        (`hPutStr` (newTodo ++ "\n"))

