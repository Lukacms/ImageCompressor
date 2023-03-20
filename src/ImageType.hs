{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- ImageType
-}

module ImageType
  ( Short,
    Color,
    Point,
    Pixel,
    Cluster,
  )
where

import Data.List (intercalate)

newtype Short = Short Int

data Color = Color Short Short Short

data Point = Point Int Int

data Pixel = Pixel Point Color

data Cluster = Cluster Color [Pixel]

instance Show Short where
  show (Short x) = show x

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Show Color where
  show (Color x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Show Pixel where
  show (Pixel x y) = show x ++ " " ++ show y

instance Show Cluster where
  show (Cluster x y) = "--\n" ++ show x ++ "\n-\n" ++ intercalate "\n" (map show y)
