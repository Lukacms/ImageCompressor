{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- Algorithm
-}

module Algorithm (
    compress
) where

import GraphicElements (Cluster(..), euclideanDistance, assignPixelsToClusters, createAverageClusters)
import Parser (Conf(..), Image (Image))
import Data.Maybe (fromJust)

compress :: Conf -> [Cluster] -> [Cluster]
compress conf clusters = compress' clusters conf []

compress' :: [Cluster] -> Conf -> [Cluster] -> [Cluster]
compress' old (Conf finalColorsNb limit (Image image)) [] = let new = createAverageClusters (assignPixelsToClusters image old) in compress' old (Conf finalColorsNb limit (Image image)) new
compress' ((Cluster c1 _):_) (Conf finalColorsNb limit (Image image)) ((Cluster c2 p2):ys)
    | euclideanDistance c1 c2 > fromIntegral (fromJust finalColorsNb) = let newer = createAverageClusters (assignPixelsToClusters image (Cluster c2 p2 : ys)) in compress' (Cluster c2 p2 : ys) (Conf finalColorsNb limit (Image image)) newer
    | otherwise = Cluster c2 p2 : ys
