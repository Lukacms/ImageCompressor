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
    getAverageColor,
    nearestCluster,
    assignPixelsToClusters,
    euclideanDistance
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
    (-) (Color (r1,g1,b1)) (Color (r2,g2,b2)) = Color (r1-r2, g1-g2, b1-b2)
    (*) (Color (r1,g1,b1)) (Color (r2,g2,b2)) = Color (r1*r2, g1*g2, b1*b2)
    abs (Color (r,g,b)) = Color (abs r, abs g, abs b)
    signum (Color (r,g,b)) = Color (signum r, signum g, signum b)
    negate (Color (r,g,b)) = Color (negate r, negate g, negate b)
    fromInteger x = Color (fromInteger x, fromInteger x, fromInteger x)

instance Show Cluster where
    show (Cluster color pixels) = "--\n" ++ show color ++ "\n-\n" ++ intercalate "\n" (map show pixels) ++ "\n"

instance Show Pixel where
  show (Pixel x y) = show x ++ " " ++ show y

instance Show Point where
  show (Point (x,y)) = "(" ++ show x ++ "," ++ show y ++ ")"

createNCluster :: Int -> [(Int, Int, Int)] -> [Cluster]
createNCluster 0 _ = []
createNCluster _ [] = []
createNCluster x (random:xs) = Cluster (Color random) [] : createNCluster (x - 1) xs

createAverageClusters :: [Cluster] -> [Cluster]
createAverageClusters [] = []
createAverageClusters ((Cluster _ pixels):xs) = Cluster (getAverageColor pixels) [] : createAverageClusters xs

nearestCluster :: Cluster -> Pixel -> [Cluster] -> [Cluster]
nearestCluster (Cluster color pixels) pixel [] = [Cluster color (pixel : pixels)]
nearestCluster (Cluster color1 pixels1) (Pixel point color2) ((Cluster color3 pixels2):xs)
    | euclideanDistance color1 color2 > euclideanDistance color3 color2 = Cluster color1 pixels1 : nearestCluster (Cluster color3 pixels2) (Pixel point color2) xs
    | otherwise = Cluster color3 pixels2 : nearestCluster (Cluster color1 pixels1) (Pixel point color2) xs

assignPixelsToClusters :: [Pixel] -> [Cluster] -> [Cluster]
assignPixelsToClusters [] clusters = clusters
assignPixelsToClusters ((Pixel point color):xs) (cl:cls) = assignPixelsToClusters xs (nearestCluster cl (Pixel point color) cls)
assignPixelsToClusters _ [] = []

euclideanDistance :: Color -> Color -> Double
euclideanDistance (Color (r1,g1,b1)) (Color (r2,g2,b2)) = sqrt ((fromIntegral r1 - fromIntegral r2)**2
    + (fromIntegral g1 - fromIntegral g2)**2 + (fromIntegral b1 - fromIntegral b2)**2)

getAverageColor :: [Pixel] -> Color
getAverageColor pixels = generateAverageColor (addAllColors pixels) (length pixels)

generateAverageColor :: Color -> Int -> Color
generateAverageColor color 0 = color
generateAverageColor (Color (r, g, b)) len = Color (r `div` len  ,g `div` len, b `div` len)

addAllColors :: [Pixel] -> Color
addAllColors [] = 0
addAllColors [Pixel _ color] = color
addAllColors ((Pixel _ color) : xs) = color + addAllColors xs
