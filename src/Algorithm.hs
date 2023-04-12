{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- Algorithm
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Algorithm (
    compress
) where

import GraphicElements (Cluster(..),
                        Pixel(..),
                        euclideanDistance, 
                        assignPixelsToClusters, 
                        createAverageClusters, )
import ArgumentsParser (Conf(..), Limit(..))
import FileParser (Image(..))

compress :: [Cluster] -> Conf -> [Cluster]
compress clusters conf = compress' conf clusters []

compress' :: Conf -> [Cluster] -> [Cluster] -> [Cluster]
compress' (Conf nbr limit path image) oldClusters []
    = compress' (Conf nbr limit path image) oldClusters $ createAverageClusters $ assignPixelsToClusters (imageToPixels image) oldClusters
compress' (Conf nbr limit path image) oldClusters newClusters 
    | isLimitReached limit oldClusters newClusters = assignPixelsToClusters (imageToPixels image) newClusters
    | otherwise = compress' (Conf nbr limit path image) (createAverageClusters (assignPixelsToClusters (imageToPixels image) newClusters))
        $ createAverageClusters $ assignPixelsToClusters (imageToPixels image) newClusters

imageToPixels :: Image -> [Pixel]
imageToPixels (Image []) = []
imageToPixels (Image ((point, color):as)) = Pixel point color : imageToPixels (Image as)
imageToPixels _ = []

isLimitReached :: Limit -> [Cluster] -> [Cluster] -> Bool
isLimitReached _ [] [] = True
isLimitReached (Limit double) ((Cluster oldColor _):old) ((Cluster newColor _):new)
  | euclideanDistance oldColor newColor <= double = isLimitReached (Limit double) old new
  | otherwise = False
