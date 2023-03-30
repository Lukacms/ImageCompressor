{-
-- EPITECH PROJECT, 2023
-- src
-- File description:
-- Help
-}

module Help (printHelp) where

printHelp :: IO ()
printHelp =
  mapM_
    putStrLn
    [ "USAGE: ./imageCompressor -n N -l L -f F\n",
      "\tN\tnumber of colors in the final image",
      "\tL\tconvergence limit",
      "\tF\tpath to the file containing the colors of the pixels"
    ]
