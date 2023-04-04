{-
-- EPITECH PROJECT, 2023
-- app
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import           Help               (printHelp)
import           ArgumentsParser    (Conf(..), FinalColorNumber(..), Filepath(..) ,defaultConf, getOpts)
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (ExitFailure), exitSuccess,
                                     exitWith)
import           RandomNumber       (createListOfRandomNumbers)
import           Algorithm          (compress)
import           GraphicElements    (createNCluster)
import           FileParser         (parse)

main:: IO ()
main = getArgs >>= launch . getOpts defaultConf

setImage:: Conf -> String -> Conf
setImage (Conf nbr limit (Filepath path) _) str = Conf nbr limit (Filepath path) (parse str) 
setImage conf _ = conf

launch:: Conf -> IO ()
launch OptsError = printHelp >> exitWith (ExitFailure 84)
launch Help      = printHelp >> exitSuccess
launch (Conf (FinalColorNumber nbr) limit path image) = createListOfRandomNumbers nbr >>= launch' (Conf (FinalColorNumber nbr) limit path image) 

launch':: Conf -> [(Int, Int, Int)] -> IO ()
launch' (Conf (FinalColorNumber nbr) limit (Filepath path) image) random = readFile path
    >>= mapM_ (putStr . show) . compress (createNCluster nbr random) . setImage (Conf (FinalColorNumber nbr) limit (Filepath path) image)
