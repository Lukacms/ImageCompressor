{-
-- EPITECH PROJECT, 2023
-- app
-- File description:
-- Main
-}

module Main (main) where

import Help (printHelp)
import Parser (Conf (..), defaultConf, getOpts)
import System.Environment (getArgs)
import System.Exit
  ( ExitCode (ExitFailure),
    exitSuccess,
    exitWith,
  )

main :: IO ()
main = getArgs >>= launch . getOpts defaultConf

-- print dans le main, launch ne return que une string à print
launch :: Conf -> IO ()
launch OptsError = printHelp >> exitWith (ExitFailure 84)
launch Help = printHelp >> exitSuccess
launch _ = exitSuccess
