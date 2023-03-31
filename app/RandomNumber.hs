{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- RandomNumber
-}

module RandomNumber (
    createListOfRandomNumbers
) where

import System.Random
import Control.Applicative

-- <*> : Permet d'effectuer les operations directement dans les monades, add <$> Just 1 <*> Just 4 = Just 5.
-- <*> : Permet de faire prendre plusieurs arguments qui sont encapsulés a une fonction.
-- (,,) : Fonction qui permet de construire un triplet (,,) 1 2 3 = (1,2,3).
-- <$> : Fmap :D.
-- return : Permet d'encapsuler la valeur dans une monade :D

createListOfRandomNumbers :: Int -> IO [(Int, Int, Int)]
createListOfRandomNumbers 0 = return []
createListOfRandomNumbers nbr = (:) <$> ((,,) <$> randomRIO (0,255) <*> randomRIO (0,255) <*> randomRIO (0,255)) <*> createListOfRandomNumbers (nbr - 1)
