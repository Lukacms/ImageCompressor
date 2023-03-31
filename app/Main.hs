{-
-- EPITECH PROJECT, 2023
-- app
-- File description:
-- Main
-}

module Main (main) where

import ArgumentsParser (Conf (Help, OptsError), defaultConf, getOpts)
import Control.Exception (handle)
import Help (printHelp)
import ICException (exceptionHandler)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

main :: IO ()
main = handle exceptionHandler $ getArgs >>= launch . getOpts defaultConf

launch :: Conf -> IO ()
launch OptsError = printHelp >> exitWith (ExitFailure 84)
launch Help = printHelp >> exitSuccess
launch _ = exitSuccess
