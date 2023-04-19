{-
-- EPITECH PROJECT, 2023
-- src
-- File description:
-- ArgumentsParser
-}

module ArgumentsParser
  ( defaultConf,
    getOpts,
    Conf (..),
    Limit (..),
    FinalColorNumber (..),
    Filepath (..),
  )
where

import Control.Exception (throw)
import FileParser (Image (ParseError))
import ICException (ICException (ArgumentException))
import Text.Read

-- useful types

newtype FinalColorNumber = FinalColorNumber Int deriving (Eq, Show)

newtype Limit = Limit Double deriving (Eq, Show)

data Filepath = Invalid | Filepath String deriving (Eq, Show)

-- useful data

data Conf
  = OptsError
  | Help
  | Conf FinalColorNumber Limit Filepath Image
  deriving (Eq, Show)

-- functions

defaultConf :: Conf
defaultConf = Conf (FinalColorNumber 0) (Limit 0) Invalid ParseError

getOpts :: Conf -> [String] -> Conf
getOpts _ ["-h"] = Help
getOpts _ [_] = OptsError
getOpts conf [] = getOpts' conf
getOpts (Conf _ limit filepath image) ("-n" : x : xs) =
  getOpts
    (Conf (FinalColorNumber $ checkValidityOfNumber x) limit filepath image)
    xs
getOpts (Conf finalColorNum _ filepath image) ("-l" : x : xs) =
  getOpts
    (Conf finalColorNum (Limit $ checkValidityOfDouble x) filepath image)
    xs
getOpts (Conf finalColorNum limit _ image) ("-f" : x : xs) =
  getOpts (Conf finalColorNum limit (Filepath x) image) xs
getOpts OptsError _ = OptsError
getOpts Help _ = Help
getOpts _ _ = OptsError

getOpts' :: Conf -> Conf
getOpts' (Conf (FinalColorNumber num) (Limit lim) (Filepath path) image)
  | num <= 0 = OptsError
  | lim <= 0 = OptsError
  | null path = OptsError
  | otherwise = Conf (FinalColorNumber num) (Limit lim) (Filepath path) image
getOpts' (Conf _ _ Invalid _) = OptsError
getOpts' _ = OptsError

checkValidityOfNumber :: String -> Int
checkValidityOfNumber numStr = case readMaybe numStr of
  (Just num) -> num
  _ -> throw ArgumentException

checkValidityOfDouble :: String -> Double
checkValidityOfDouble numStr = case readMaybe numStr of
  (Just num) -> num
  _ -> throw ArgumentException