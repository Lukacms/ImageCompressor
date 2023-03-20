{-
-- EPITECH PROJECT, 2023
-- src
-- File description:
-- Parser
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Parser (Point(..), Color(..), getOpts, Image(..), defaultConf) where

import           Foreign.Marshal.Unsafe
import           Text.Read              (readMaybe)

newtype Point   = Point (Int, Int)
newtype Color   = Color (Int, Int, Int)

data Image  = ParseError | Image [(Point, Color)]

data Conf   = OptsError | Conf {
    finalColorsNb :: Maybe Int,
    limit         :: Maybe Double,
    image         :: Image
}

defaultConf:: Conf
defaultConf = Conf {
    finalColorsNb = Nothing,
    limit = Nothing,
    image = ParseError
}

readPoint:: String -> Maybe Point
readPoint x = case readMaybe x of
    (Just(a, b)) -> Just (Point (a, b))
    _            -> Nothing

readColor:: String -> Maybe Color
readColor x = case readMaybe x of
    (Just(a, b, c)) -> Just (Color (a, b, c))
    _               -> Nothing

readLine:: [String] -> Maybe (Point, Color)
readLine (x:s:_) = case (readPoint x, readColor s) of
    (Just a, Just b) -> Just (a, b)
    _                -> Nothing
readLine [] = Nothing
readLine _ = Nothing

parseFile:: [String] -> Image
parseFile []     = Image []
parseFile (x:xs) = case readLine (words x) of
    Just line -> case parseFile xs of
        ParseError  -> ParseError
        (Image img) -> Image (line : img)
    _         -> ParseError

parse:: String -> Image
parse a = parseFile (lines a)

checkConf:: Conf -> Conf
checkConf (Conf Nothing _ _)    = OptsError
checkConf (Conf _ Nothing _)    = OptsError
checkConf (Conf _ _ ParseError) = OptsError
checkConf conf                  = conf

getOpts:: Conf -> [String] -> Conf
getOpts _ [_]            = OptsError
getOpts conf []          = checkConf conf
getOpts OptsError _      = OptsError
getOpts conf ("-n":x:xs) = getOpts conf{finalColorsNb = readMaybe x} xs
getOpts conf ("-l":x:xs) = getOpts conf{limit = readMaybe x} xs
getOpts conf ("-f":x:xs) =
    let loadedImage = fmap parse (readFile x)
    in getOpts conf{image = unsafeLocalState loadedImage} xs
getOpts _  _             = OptsError
