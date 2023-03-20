{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- ImageType
-}

module ImageType (
    Color(..),
    Point(..),
    Pixel(..),
    Cluster(..)
) where

import Data.List (intercalate)

newtype Point = Point (Int, Int) deriving (Eq)

newtype Color = Color (Int, Int, Int) deriving (Eq)

data Pixel = Pixel Point Color deriving (Eq)

data Cluster = Cluster Color [Pixel] deriving (Eq)

instance Show Point where
  show (Point (x,y)) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Show Color where
  show (Color (x,y,z)) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Show Pixel where
  show (Pixel x y) = show x ++ " " ++ show y

instance Show Cluster where
    show (Cluster x y) = "--\n" ++ show x ++ "\n-\n" ++ intercalate "\n" (map show y)

instance Num Color where
    (+) (Color (a,b,c)) (Color (d,e,f)) = Color (a+d, b+e, c+f)

