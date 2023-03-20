{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- Algorithm
-}

import ImageType (Cluster(..), Color(..), Pixel(..))
import System.Random ()

generateNCluster :: Int -> [Cluster]
generateNCluster 0 = []
generateNCluster x = Cluster (Color (255,255,255)) [] : generateNCluster (x - 1)

generateAverageColor :: [Pixel] -> Color
generateAverageColor [Pixel _ b] = b
generateAverageColor ((Pixel _ b) : xs) = b + generateAverageColor xs

generateAverageCluster :: Cluster -> Cluster
generateAverageCluster (Cluster _ b) = Cluster (generateAverageColor b) []

getDistance :: Color -> Color -> Double
getDistance (Color (r1,g1,b1)) (Color (r2,g2,b2)) = sqrt ((fromIntegral r1 - fromIntegral r2)^2
    + (fromIntegral g1 - fromIntegral g2)^2 + (fromIntegral b1 - fromIntegral b2)^2)
