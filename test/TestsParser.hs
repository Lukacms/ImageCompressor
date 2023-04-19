{-
-- EPITECH PROJECT, 2023
-- test
-- File description:
-- TestsParser
-}

module TestsParser (testParser) where

import Parser (Conf (..), Point (Point), defaultConf)
import Test.Hspec

testParser :: Spec
testParser =
  describe "Parser test" $
    it "Should do nothing much" (defaultConf `shouldNotBe` OptsError)
      >> testParserPoint

testParserPoint :: Spec
testParserPoint = it "Should test Point" (Point (1, 2) `shouldBe` Point (1, 2))
