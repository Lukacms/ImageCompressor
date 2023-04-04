{-
-- EPITECH PROJECT, 2023
-- src
-- File description:
-- FileParser
-}

module FileParser (Image (..), parse) where

import Text.Read

-- useful types

import GraphicElements ( Color(..), Point(..) )

-- useful data

data Image = ParseError | Image [(Point, Color)] deriving (Show)

-- function
parse :: String -> Image
parse a = parseFile (lines a)

parseFile :: [String] -> Image
parseFile [] = Image []
parseFile (x : xs) = Image (readLine (words x) : parseFile' (parseFile xs))

parseFile' :: Image -> [(Point, Color)]
parseFile' (Image img) = img
parseFile' ParseError = [] -- throw error

readLine :: [String] -> (Point, Color)
readLine (x : s : _) = (readPoint x, readColor s)
readLine [] = (Point (0, 0), Color (0, 0, 0))
readLine _ = (Point (0, 0), Color (0, 0, 0))

readPoint :: String -> Point
readPoint x = case readMaybe x of
  (Just (a, b)) -> Point (a, b)
  _ -> Point (0, 0) -- throw an error

readColor :: String -> Color
readColor x = case readMaybe x of
  (Just (a, b, c)) -> Color (a, b, c)
  _ -> Color (0, 0, 0) -- throw an error
