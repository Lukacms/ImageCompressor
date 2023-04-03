{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- Exception
-}

module Exception () where

data ImgCompressorException = ArgumentException 
                            | ImageFormatException 
                            | RuntimeException

instance Exception ImgCompressorException where


-- throw ArgumentException

exceptionHandler :: ImgCompressorException -> IO ()
exceptionHandler ArgumentException      = printLn "TTOTO" >> exitWith $ ExitFailure 84
exceptionHandler ImageFormatException   = return ()
exceptionHandler RuntimeException       = return ()