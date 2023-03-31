{-
-- EPITECH PROJECT, 2023
-- app
-- File description:
-- Main
-}

module Main (main) where

import           Help               (printHelp)
import           Parser             (Conf(..), defaultConf, getOpts)
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (ExitFailure), exitSuccess,
                                     exitWith)
import           RandomNumber       (createListOfRandomNumbers)
import           Algorithm          (compress)
import           GraphicElements    (createNCluster)
import           Data.Maybe         (fromJust)

main:: IO ()
main = getArgs >>= launch . getOpts defaultConf

launch:: Conf -> IO ()
launch OptsError = printHelp >> exitWith (ExitFailure 84)
launch Help      = printHelp >> exitSuccess
launch conf      = createListOfRandomNumbers (fromJust (finalColorsNb conf)) >>= mapM_ (putStr . show) . compress conf . createNCluster (fromJust (finalColorsNb conf))
