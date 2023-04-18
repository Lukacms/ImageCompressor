{-
-- EPITECH PROJECT, 2023
-- app
-- File description:
-- Exception
-}

module ICException (exceptionHandler, ICException (..)) where

import Control.Exception
import System.Exit (ExitCode (ExitFailure), exitWith)

data ICException
  = ArgumentException
  | ImageFormatException
  | RuntimeException
  deriving (Show)

-- il existe les fonctions throw et catch, qui prend une exception et return un IO (a)
-- on utilise catch pour le "petit" scope, exeption à courte portée
-- handle = même fonction que catch MAIS prend en 1e le handler puis le code -> plus lisible
-- il faut définir une fonction de handling

instance Exception ICException

exceptionHandler :: ICException -> IO ()
exceptionHandler ArgumentException = exitWith $ ExitFailure 84
exceptionHandler ImageFormatException = exitWith $ ExitFailure 84
exceptionHandler RuntimeException = exitWith $ ExitFailure 84
