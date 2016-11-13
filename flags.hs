module Flags(getFlags) where

type Flag = String
type FlagArg = Maybe String

getFlags :: [String] -> [(Flag, FlagArg)]
getFlags = map (\flag -> (parseFlag flag, if getFlagArg flag == []
                                   then Nothing
                                   else Just (getFlagArg flag)
                         )
               ) .
        filter isFlag
    where
        isFlag ('-':rest) = True
        isFlag _ = False
        parseFlag = takeWhile (/='=') . dropWhile (=='-')
        getFlagArg = drop 1 . dropWhile (/='=')
