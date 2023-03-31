{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- Cluster
-}

module GraphicElements (
    Cluster(..),
    Pixel(..),
    Point(..),
    Color(..),
    createNCluster,
    createAverageClusters,
    nearestCluster,
    assignPixelsToClusters,
    createAverageColor,
    euclideanDistance,
) where

import Data.List (intercalate)

data Cluster = Cluster Color [Pixel] deriving (Eq)

data Pixel = Pixel Point Color deriving (Eq)

newtype Point = Point (Int, Int) deriving (Eq)

newtype Color = Color (Int, Int, Int) deriving (Eq)

instance Show Color where
  show (Color (x,y,z)) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Num Color where
    (+) (Color (r1,g1,b1)) (Color (r2,g2,b2)) = Color (r1+r2, g1+g2, b1+b2)
    (*) (Color (r1,g1,b1)) (Color (r2,g2,b2)) = Color (r1*r2, g1*g2, b1*b2)
    abs (Color (r,g,b)) = Color (abs r, abs g, abs b)

instance Show Cluster where
    show (Cluster color pixels) = "--\n" ++ show color ++ "\n-\n" ++ intercalate "\n" (map show pixels)

instance Show Pixel where
  show (Pixel x y) = show x ++ " " ++ show y

instance Show Point where
  show (Point (x,y)) = "(" ++ show x ++ "," ++ show y ++ ")"

createNCluster :: Int -> [(Int, Int, Int)] -> [Cluster]
createNCluster 0 _ = []
createNCluster x random:xs = Cluster (Color random) [] : createNCluster (x - 1) xs

createAverageClusters :: [Cluster] -> [Cluster]
createAverageClusters [] = []
createAverageClusters ((Cluster _ pixels):xs) = Cluster (createAverageColor pixels) [] : createAverageClusters xs

nearestCluster :: Cluster -> Pixel -> [Cluster] -> [Cluster]
nearestCluster (Cluster color pixels) pixel [] = [Cluster color (pixel : pixels)]
nearestCluster (Cluster a b) (Pixel c d) ((Cluster e f):xs)
    | euclideanDistance a d > euclideanDistance e d = Cluster a b : nearestCluster (Cluster e f) (Pixel c d) xs
    | otherwise = Cluster e f : nearestCluster (Cluster a b) (Pixel c d) xs

assignPixelsToClusters :: [(Point, Color)] -> [Cluster] -> [Cluster]
assignPixelsToClusters [] clusters = clusters
assignPixelsToClusters ((p, c): xs) (cl:cls) = assignPixelsToClusters xs (nearestCluster cl (Pixel p c) cls)

euclideanDistance :: Color -> Color -> Double
euclideanDistance (Color (r1,g1,b1)) (Color (r2,g2,b2)) = sqrt ((fromIntegral r1 - fromIntegral r2)^2
    + (fromIntegral g1 - fromIntegral g2)^2 + (fromIntegral b1 - fromIntegral b2)^2)

createAverageColor :: [Pixel] -> Color
createAverageColor [Pixel _ color] = color
createAverageColor ((Pixel _ color) : xs) = color + createAverageColor xs
