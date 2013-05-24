{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.ParseSpec (spec) where

import Text.Karver.Parse
import Text.Karver.Types

import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import Test.Hspec

literal, ident :: Text -> Either String Tokens
literal = parseOnly literalParser

ident = parseOnly identityParser

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

spec :: Spec
spec = do
  describe "literalParser" $ do
    it "no input" $ do
      let noText = ""
          value  = literal noText

      value `shouldSatisfy` isLeft

    it "stops at first {" $ do
      let text     = "a{ should no parse"
          value    = literal text
          expected = Right $ Literal "a"

      value `shouldBe` expected

    it "untill the end" $ do
      let fullText = "all this text is here"
          value    = literal fullText
          expected = Right $ Literal fullText

      value `shouldBe` expected

  describe "identityParser" $ do
    it "no input" $ do
      let noText   = ""
          value    = ident noText

      value `shouldSatisfy` isLeft

    it "regular identity" $ do
      let regText  = "{{ name }}"
          value    = ident regText
          expected = Right $ Identity "name"

      value `shouldBe` expected

    it "no spaces identity" $ do
      let regText  = "{{name}}"
          value    = ident regText
          expected = Right $ Identity "name"

      value `shouldBe` expected

    it "no space on right identity" $ do
      let rText    = "{{ name}}"
          value    = ident rText
          expected = Right $ Identity "name"

      value `shouldBe` expected

    it "no space on left identity" $ do
      let lText    = "{{name }}"
          value    = ident lText
          expected = Right $ Identity "name"

      value `shouldBe` expected

    it "multiple spaces identity" $ do
      let multiText = "{{     name   }}"
          value     = ident multiText
          expected  = Right $ Identity "name"

      value `shouldBe` expected
