{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.ParseSpec (spec) where

import Text.Karver.Parse
import Text.Karver.Types

import Prelude hiding (concat, unlines)
import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text, concat, empty, pack, unlines)
import Test.Hspec

literal, variable, condition, loop, include
  :: Text -> Either String Token
literal   = parseOnly literalParser
variable  = parseOnly variableParser
condition = parseOnly conditionParser
loop      = parseOnly loopParser
include   = parseOnly includeParser

noDemVariable :: Text -> Either String Token
noDemVariable = parseOnly variableParser'

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

spec :: Spec
spec = do
  describe "literalParser" $ do
    it "should return Left with an empty string" $ do
      let noText = empty
          value  = literal noText

      value `shouldSatisfy` isLeft

    it "should continue parsing after `{`" $ do
      let text     = "a{ should parse"
          value    = literal text
          expected = Right $ LiteralTok text

      value `shouldBe` expected

    it "should stop parsing on `{{`" $ do
      let text     = "a{{ should not parse"
          value    = literal text
          expected = Right $ LiteralTok "a"

      value `shouldBe` expected

    it "should stop parsing on `{%`" $ do
      let text     = "a{% should not parse"
          value    = literal text
          expected = Right $ LiteralTok "a"

      value `shouldBe` expected

    it "should not fail with `{` at the end" $ do
      let text     = "a sentence with {"
          value    = literal text
          expected = Right $ LiteralTok text

      value `shouldBe` expected

    it "should parse all the way to the end" $ do
      let fullText = "all this text is here"
          value    = literal fullText
          expected = Right $ LiteralTok fullText

      value `shouldBe` expected

  describe "identityParser" $ do
    it "should return Left with an empty string" $ do
      let noText   = empty
          value    = variable noText

      value `shouldSatisfy` isLeft

    it "should parse identity with spaces" $ do
      let regText  = "{{ name }}"
          value    = variable regText
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "should parse identity without spaces" $ do
      let regText  = "{{name}}"
          value    = variable regText
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "should parse identity with space to the left" $ do
      let rText    = "{{ name}}"
          value    = variable rText
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "should parse identity with space to the right" $ do
      let lText    = "{{name }}"
          value    = variable lText
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "should parse identity with multiple spaces" $ do
      let multiText = "{{     name   }}"
          value     = variable multiText
          expected  = Right $ IdentityTok "name"

      value `shouldBe` expected

  describe "objectParser" $ do
    it "should parse normal object variables" $ do
      let regObj   = "{{ person.name }}"
          value    = variable regObj
          expected = Right $ ObjectTok "person" "name"

      value `shouldBe` expected

  describe "arrayParser" $ do
    it "should parse normal array variables" $ do
      let regList  = "{{ names[1] }}"
          value    = variable regList
          expected = Right $ ListTok "names" 1

      value `shouldBe` expected

    it "should parse array with max number index" $ do
      let maxInt   = maxBound
          regList  = concat [ "{{ names["
                            , (pack $ show maxInt)
                            , "] }}"
                            ]
          value    = variable regList
          expected = Right $ ListTok "names" maxInt

      value `shouldBe` expected

  describe "conditionParser" $ do
    it "should parse single line if statement" $ do
      let ifText    = "{% if title %}{{ title }}{% endif %}"
          value     = condition ifText
          expected  = Right $ ConditionTok "title" "{{ title }}" empty

      value `shouldBe` expected

    it "should parse multi line if statement" $ do
      let ifText    = unlines [ "{% if title %}"
                              , "  {{ title }}"
                              , "{% endif %}"
                              ]
          value     = condition ifText
          expected  = Right $ ConditionTok "title" "  {{ title }}\n" empty

      value `shouldBe` expected

    it "should parse single line if else statement" $ do
      let ifelse   = concat [ "{% if title %}{{ title }}{% else %}"
                            , "no title{% endif %}"
                            ]
          value    = condition ifelse
          expected = Right $ ConditionTok "title" "{{ title }}" "no title"

      value `shouldBe` expected

    it "should parse multi line if else statement" $ do
      let ifText    = unlines [ "{% if title %}"
                              , "  {{ title }}"
                              , "{% else %}"
                              , "  title"
                              , "{% endif %}"
                              ]
          value     = condition ifText
          expected  = Right $ ConditionTok "title"
                                           "  {{ title }}\n"
                                           "  title\n"

      value `shouldBe` expected

  describe "loopParser" $ do
    it "should parse single line for loop" $ do
      let loopText = concat [ "{% for item in items %}"
                            , "  {{ item }}"
                            , "{% endfor %}"
                            ]
          value    = loop loopText
          expected = Right $ LoopTok "items" "item" "  {{ item }}"

      value `shouldBe` expected

    it "should parse multi line for loop" $ do
      let loopText = unlines [ "{% for item in items %}"
                             , "  {{ item }}"
                             , "{% endfor %}"
                             ]
          value    = loop loopText
          expected = Right $ LoopTok "items" "item" "  {{ item }}\n"

      value `shouldBe` expected

  describe "includeParser" $ do
    it "should import file with single quotes" $ do
      let includeText = "{% include 'template.html' %}"
          value       = include includeText
          expected    = Right $ IncludeTok "template.html"

      value `shouldBe` expected

    it "should import file with double quotes" $ do
      let includeText = "{% include \"template.html\" %}"
          value       = include includeText
          expected    = Right $ IncludeTok "template.html"

      value `shouldBe` expected

  describe "no delimiter" $ do
    it "should parse identity variable" $ do
      let value    = noDemVariable "name"
          expected = Right $ IdentityTok "name"

      value `shouldBe` expected

    it "should parse object variable" $ do
      let value    = noDemVariable "project.name"
          expected = Right $ ObjectTok "project" "name"

      value `shouldBe` expected

    it "should  parse list variable" $ do
      let value    = noDemVariable "names[4]"
          expected = Right $ ListTok "names" 4

      value `shouldBe` expected
