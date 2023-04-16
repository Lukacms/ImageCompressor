{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- Cluster
-}

module GraphicElements
  ( Cluster (..),
    Pixel (..),
    Point (..),
    Color (..),
    createNCluster,
    createAverageClusters,
    getAverageColor,
    nearestCluster,
    assignPixelsToClusters,
    euclideanDistance,
  )
where

import Data.List (intercalate)

data Cluster = Cluster Color [Pixel] deriving (Eq)

data Pixel = Pixel Point Color deriving (Eq)

newtype Point = Point (Int, Int) deriving (Eq)

newtype Color = Color (Int, Int, Int) deriving (Eq)

instance Show Color where
  show (Color (x, y, z)) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Num Color where
  (+) (Color (r1, g1, b1)) (Color (r2, g2, b2)) = Color (r1 + r2, g1 + g2, b1 + b2)
  (-) (Color (r1, g1, b1)) (Color (r2, g2, b2)) = Color (r1 - r2, g1 - g2, b1 - b2)
  (*) (Color (r1, g1, b1)) (Color (r2, g2, b2)) = Color (r1 * r2, g1 * g2, b1 * b2)
  abs (Color (r, g, b)) = Color (abs r, abs g, abs b)
  signum (Color (r, g, b)) = Color (signum r, signum g, signum b)
  negate (Color (r, g, b)) = Color (negate r, negate g, negate b)
  fromInteger x = Color (fromInteger x, fromInteger x, fromInteger x)

instance Show Cluster where
  show (Cluster color pixels) = "--\n" ++ show color ++ "\n-\n" ++
    intercalate "\n" (map show pixels) ++ "\n"

instance Show Pixel where
  show (Pixel x y) = show x ++ " " ++ show y

instance Show Point where
  show (Point (x, y)) = "(" ++ show x ++ "," ++ show y ++ ")"

createNCluster :: Int -> [(Int, Int, Int)] -> [Cluster]
createNCluster 0 _ = []
createNCluster _ [] = []
createNCluster x (random : xs) =
                    Cluster (Color random) [] : createNCluster (x - 1) xs

createAverageClusters :: [Cluster] -> [Cluster]
createAverageClusters [] = []
createAverageClusters ((Cluster color []) : xs) =
    Cluster color [] : createAverageClusters xs
createAverageClusters ((Cluster _ pixels) : xs) =
    Cluster (getAverageColor pixels) [] : createAverageClusters xs

nearestCluster :: Pixel -> [Cluster] -> [Cluster] -> [Cluster]
nearestCluster _ [] _ = []
nearestCluster pixel ((Cluster color pix) : cls) cl
  | nearestCluster' pixel cl == Cluster color pix =
        Cluster color (pixel : pix) : nearestCluster pixel cls cl
  | otherwise = Cluster color pix : nearestCluster pixel cls cl

nearestCluster' :: Pixel -> [Cluster] -> Cluster
nearestCluster' (Pixel _ color) [Cluster color1 pix1, Cluster color2 pix2]
  | euclideanDistance color color1 < euclideanDistance color color2 =
            Cluster color1 pix1
  | otherwise = Cluster color2 pix2
nearestCluster' (Pixel point color) ((Cluster color1 pix1) :
                                    (Cluster color2 pix2) : cls)
  | euclideanDistance color color1 < euclideanDistance color color2 =
      nearestCluster' (Pixel point color) $ Cluster color1 pix1 : cls
  | otherwise = nearestCluster' (Pixel point color) $ Cluster color2 pix2 : cls
nearestCluster' _ _ = Cluster (Color (0, 0, 0)) []

assignPixelsToClusters :: [Pixel] -> [Cluster] -> [Cluster]
assignPixelsToClusters [] clusters = clusters
assignPixelsToClusters (pixel : ps) clusters =
    assignPixelsToClusters ps $ nearestCluster pixel clusters clusters

euclideanDistance :: Color -> Color -> Double
euclideanDistance (Color (r1, g1, b1)) (Color (r2, g2, b2)) =
    sqrt ((fromIntegral r1 - fromIntegral r2) ** 2 +
    (fromIntegral g1 - fromIntegral g2) ** 2 +
    (fromIntegral b1 - fromIntegral b2) ** 2)

getAverageColor :: [Pixel] -> Color
getAverageColor pxls = generateAverageColor (addAllColors pxls) (length pxls)

generateAverageColor :: Color -> Int -> Color
generateAverageColor _ 0 = 0
generateAverageColor (Color (r, g, b)) len =
    Color (r `div` len, g `div` len, b `div` len)

addAllColors :: [Pixel] -> Color
addAllColors [] = 0
addAllColors [Pixel _ color] = color
addAllColors ((Pixel _ color) : xs) = color + addAllColors xs
