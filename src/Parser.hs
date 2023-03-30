{-
-- EPITECH PROJECT, 2023
-- src
-- File description:
-- Parser
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Parser
  ( Point (..),
    Color (..),
    getOpts,
    Image (..),
    defaultConf,
    Conf (..),
  )
where

import Text.Read (readMaybe)

-- éviter écriture passive: une fonction => une action
-- mettre les fonctions de bas en haut
-- éviter d'utiliser des let in => le faire en fonction

-- function, function' qui continue l'exécution de la première fonction

-- deriving Show => intérêt que pour le débug
-- si on utilise pas une ressource, ne pas l'utiliser (pas code de prod)

newtype Point = Point (Int, Int)

newtype Color = Color (Int, Int, Int)

data Image = ParseError | Image [(Point, Color)]

newtype FinalColorNumber = FinalColorNumber Int

newtype Limit = Limit Int

-- vraiment pas ouf
-- s'appuie sur C/C++ donc pas les principes fonctionnels
-- à utiliser juste pour du web ou une interface avec c++
-- faire un type ColorNumber, limit
data Conf
  = OptsError
  | Help
  | Conf FinalColorNumber Limit Image

defaultConf :: Conf
defaultConf =
  Conf (FinalColorNumber 0) (Limit 0) ParseError

readPoint :: String -> Maybe Point
readPoint x = case readMaybe x of
  (Just (a, b)) -> Just (Point (a, b))
  _ -> Nothing

readColor :: String -> Maybe Color
readColor x = case readMaybe x of
  (Just (a, b, c)) -> Just (Color (a, b, c))
  _ -> Nothing

readLine :: [String] -> Maybe (Point, Color)
readLine (x : s : _) = case (readPoint x, readColor s) of
  (Just a, Just b) -> Just (a, b)
  _ -> Nothing
readLine [] = Nothing
readLine _ = Nothing

parseFile :: [String] -> Image
parseFile [] = Image []
parseFile (x : xs) = case readLine (words x) of
  Just line -> case parseFile xs of
    ParseError -> ParseError
    (Image img) -> Image (line : img)
  _ -> ParseError

parse :: String -> Image
parse a = parseFile (lines a)

checkConf :: Conf -> Conf
{- checkConf (Conf Nothing _ _) = OptsError
checkConf (Conf _ Nothing _) = OptsError -}
checkConf (Conf _ _ ParseError) = OptsError
checkConf conf = conf

getOpts :: Conf -> [String] -> Conf
getOpts _ ["-h"] = Help
getOpts _ [_] = OptsError
getOpts conf [] = checkConf conf
getOpts OptsError _ = OptsError
getOpts Help _ = Help
{- getOpts conf ("-n" : x : xs) = getOpts conf {finalColorsNb = readMaybe x} xs
getOpts conf ("-l" : x : xs) = getOpts conf {limit = readMaybe x} xs -}
{- getOpts conf ("-f" : x : xs) =
  let loadedImage = fmap parse (readFile x)
   in getOpts conf {image = unsafeLocalState loadedImage} xs -}
getOpts _ _ = OptsError
