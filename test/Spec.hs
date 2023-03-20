{-
-- EPITECH PROJECT, 2023
-- test
-- File description:
-- Spec
-}

module Main (main) where

import Test.Hspec (hspec)
import TestsParser (testParser)

main :: IO ()
main = hspec $ testParser
