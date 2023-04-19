{-
-- EPITECH PROJECT, 2023
-- test
-- File description:
-- TestsParser
-}

module TestsParser (testParser) where

import ArgumentsParser (Conf (..), defaultConf)
import Test.Hspec

testParser :: Spec
testParser =
  describe "Parser test" $
    it "Should do nothing much" (defaultConf `shouldNotBe` OptsError)
