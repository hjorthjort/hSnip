module Flags(
    getFlags,
    isSet,
    getFlagArg
            )
    where

import Data.Map as Map

type Flag = String
type FlagArg = Maybe String
type FlagList = Map Flag FlagArg

getFlags :: [String] -> FlagList
getFlags = fromList . Prelude.map (\flag -> (parseFlag flag, if getFlagArg flag == []
                                   then Nothing
                                   else Just (getFlagArg flag)
                         )
               ) .
        Prelude.filter isFlag
    where
        isFlag ('-':rest) = True
        isFlag _ = False
        parseFlag = takeWhile (/='=') . dropWhile (=='-')
        getFlagArg = drop 1 . dropWhile (/='=')

isSet :: Flag -> FlagList -> Bool
isSet f fs
  | Map.lookup f fs == Nothing = False
  | otherwise = True

-- | Get argument for first occurence of given flag.
getFlagArg :: Flag -> FlagList -> FlagArg
getFlagArg f fs | isSet f fs = (\(Just a) -> a) $ Map.lookup f fs
                | otherwise = Nothing

-- | Delete first occurence of given flag.
