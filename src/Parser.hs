{-
-- EPITECH PROJECT, 2023
-- src
-- File description:
-- Parser
-}

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

parseFile:: [String] -> Image
parseFile [] = Image []
parseFile _  = ParseError

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
