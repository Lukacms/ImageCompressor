{-
-- EPITECH PROJECT, 2023
-- app
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Algorithm (compress)
import ArgumentsParser
  ( Conf (..),
    Filepath (..),
    FinalColorNumber (..),
    defaultConf,
    getOpts,
  )
import Control.Exception
import FileParser (parse)
import GraphicElements (createNCluster)
import Help (printHelp)
import ICException (exceptionHandler)
import RandomNumber (createListOfRandomNumbers)
import System.Environment (getArgs)
import System.Exit
  ( ExitCode (ExitFailure),
    exitSuccess,
    exitWith,
  )

main :: IO ()
main = handle exceptionHandler $ getArgs >>= launch . getOpts defaultConf

setImage :: Conf -> String -> Conf
setImage (Conf nbr limit (Filepath path) _) str =
  Conf nbr limit (Filepath path) (parse str)
setImage conf _ = conf

launch :: Conf -> IO ()
launch OptsError = printHelp >> exitWith (ExitFailure 84)
launch Help = printHelp >> exitSuccess
launch (Conf (FinalColorNumber nbr) limit path image) =
  createListOfRandomNumbers nbr
    >>= launch' (Conf (FinalColorNumber nbr) limit path image)

launch' :: Conf -> [(Int, Int, Int)] -> IO ()
launch' (Conf (FinalColorNumber nbr) limit (Filepath path) image) random =
  readFile path
    >>= mapM_ (putStr . show)
      . compress (createNCluster nbr random)
      . setImage (Conf (FinalColorNumber nbr) limit (Filepath path) image)
