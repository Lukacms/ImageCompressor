{-
-- EPITECH PROJECT, 2023
-- src
-- File description:
-- ArgumentsParser
-}

module ArgumentsParser (defaultConf, getOpts) where

import FileParser (Image (ParseError))
import Text.Read

-- useful types

newtype FinalColorNumber = FinalColorNumber Int

newtype Limit = Limit Int

data Filepath = Invalid | Filepath String

-- useful data

data Conf
  = OptsError
  | Help
  | Conf FinalColorNumber Limit Filepath Image

-- functions

defaultConf :: Conf
defaultConf = Conf (FinalColorNumber 0) (Limit 0) Invalid ParseError

getOpts :: Conf -> [String] -> Conf
getOpts _ ["-h"] = Help
getOpts _ [_] = OptsError
getOpts conf [] = conf
getOpts (Conf _ limit filepath image) ("-n" : x : xs) =
  getOpts
    (Conf (FinalColorNumber $ checkValidityOfNumber x) limit filepath image)
    xs
getOpts (Conf finalColorNum _ filepath image) ("-l" : x : xs) =
  getOpts
    (Conf finalColorNum (Limit $ checkValidityOfNumber x) filepath image)
    xs
getOpts (Conf finalColorNum limit _ image) ("-f" : x : xs) =
  getOpts (Conf finalColorNum limit (Filepath x) image) xs
getOpts OptsError _ = OptsError
getOpts Help _ = Help
getOpts _ _ = OptsError

checkValidityOfNumber :: String -> Int
checkValidityOfNumber numStr = case readMaybe numStr of
  (Just num) -> num
  _ -> 0 -- throw error
