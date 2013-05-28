{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.ParseSpec (spec) where

import Text.Karver.Parse
import Text.Karver.Types

import Prelude hiding (concat, unlines)
import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text, concat, pack, unlines)
import Test.Hspec

literal, variable, condition :: Text -> Either String Tokens
literal   = parseOnly literalParser
variable  = parseOnly variableParser
condition = parseOnly conditionParser

noDemVariable :: Text -> Either String Tokens
noDemVariable = parseOnly variableParser'

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
          expected = Right $ LiteralTok "a"

      value `shouldBe` expected

    it "until the end" $ do
      let fullText = "all this text is here"
          value    = literal fullText
          expected = Right $ LiteralTok fullText

      value `shouldBe` expected

  describe "identityParser" $ do
    it "no input" $ do
      let noText   = ""
          value    = variable noText

      value `shouldSatisfy` isLeft

    it "regular identity" $ do
      let regText  = "{{ name }}"
          value    = variable regText
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "no spaces identity" $ do
      let regText  = "{{name}}"
          value    = variable regText
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "no space on right identity" $ do
      let rText    = "{{ name}}"
          value    = variable rText
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "no space on left identity" $ do
      let lText    = "{{name }}"
          value    = variable lText
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "multiple spaces identity" $ do
      let multiText = "{{     name   }}"
          value     = variable multiText
          expected  = Right $ IdentityTok "name"

      value `shouldBe` expected

  describe "objectParser" $ do
    it "no object present" $ do
      let noObj = "{{ name }}"
          value = variable noObj

      value `shouldSatisfy` isLeft

    it "regular object" $ do
      let regObj   = "{{ person.name }}"
          value    = variable regObj
          expected = Right $ ObjectTok "person" "name"

      value `shouldBe` expected

  describe "arrayParser" $ do
    it "no array present" $ do
      let noList = "{{ name }}"
          value  = variable noList

      value `shouldSatisfy` isLeft

    it "regular array" $ do
      let regList  = "{{ names[1] }}"
          value    = variable regList
          expected = Right $ ListTok "names" 1

      value `shouldBe` expected

    it "maxBound index array" $ do
      let maxInt   = maxBound
          regList  = concat [ "{{ names["
                            , (pack $ show maxInt)
                            , "] }}"
                            ]
          value    = variable regList
          expected = Right $ ListTok "names" maxInt

      value `shouldBe` expected

    it "single line if statement" $ do
      let ifText    = "{% if title %}{{ title }}{% endif %}"
          value     = condition ifText
          expected  = Right $ ConditionTok "title" "{{ title }}" ""

      value `shouldBe` expected

    it "multi line if statement" $ do
      let ifText    = unlines [ "{% if title %}"
                              , "  {{ title }}"
                              , "{% endif %}"
                              ]
          value     = condition ifText
          expected  = Right $ ConditionTok "title" "{{ title }}\n" ""

      value `shouldBe` expected

  describe "no delimiter" $ do
    it "identity" $ do
      let value    = noDemVariable "name"
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "object" $ do
      let value    = noDemVariable "project.name"
          expected = Right $ ObjectTok "project" "name"

      value `shouldBe` expected

    it "list" $ do
      let value    = noDemVariable "names[4]"
          expected = Right $ ListTok "names" 4

      value `shouldBe` expected
