module Flags(
    getFlags,
    isSet,
    getFlagArg
            )
    where

type Flag = String
type FlagArg = Maybe String
type FlagList = [(Flag, FlagArg)]

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

isSet :: Flag -> FlagList -> Bool
isSet f fs
  | lookup f fs == Nothing = False
  | otherwise = True

getFlagArg :: Flag -> FlagList -> FlagArg
getFlagArg f fs | isSet f fs = (\(Just a) -> a) $ lookup f fs
                | otherwise = Nothing
