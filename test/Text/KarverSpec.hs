{-# LANGUAGE OverloadedStrings #-}

module Text.KarverSpec (spec) where

import Text.Karver
import Text.Karver.Types

import Prelude hiding (unlines)
import Data.HashMap.Strict (fromList)
import Data.Text (Text, append, unlines)
import qualified Data.Vector as V
import Test.Hspec

renderer :: Text -> Text
renderer = renderTemplate
  (fromList $ [ ("project",     String "karver")
              , ("language",    String "haskell")
              , ("ver-control", String "git")
              , ("template",    Map $ fromList
                                  [ ("name", "karver")])
              , ("libraries",   List $ V.fromList
                                  ["attoparsec", "hspec"])
              ])

spec :: Spec
spec = do
  describe "renderTemplate" $ do
    it "identity at the end" $ do
      let endText  = "Template engine named {{ project }}"
          value    = renderer endText
          expected = "Template engine named karver"

      value `shouldBe` expected

    it "identity at the beginning" $ do
      let beginText = "{{ language }} is what we are written in."
          value     = renderer beginText
          expected  = "haskell is what we are written in."

      value `shouldBe` expected

    it "identity in the middle" $ do
      let middleText = "All kept in a {{ ver-control }} repo, on Github."
          value      = renderer middleText
          expected   = "All kept in a git repo, on Github."

      value `shouldBe` expected

    it "multiple identities" $ do
      let multiText = append "{{ project }} is written in {{ language }}"
                             ", held in {{ ver-control }}."
          value     = renderer multiText
          expected  = "karver is written in haskell, held in git."

      value `shouldBe` expected

    it "multiple line of identities" $ do
      let multiText = unlines
                        [ "{{ project }} is the name"
                        , "making template is my game"
                        , "if need something done faster"
                        , "you need something written in {{ language }}"
                        ]
          value     = renderer multiText
          expected  = unlines
                        [ "karver is the name"
                        , "making template is my game"
                        , "if need something done faster"
                        , "you need something written in haskell"
                        ]

      value `shouldBe` expected

    it "object identity" $ do
      let objText  = "Templating with {{ template.name }} is easy."
          value    = renderer objText
          expected = "Templating with karver is easy."

      value `shouldBe` expected

    it "mix of object a identity #1" $ do
      let mixText  = "My {{ project }} is your {{ template.name }}."
          value    = renderer mixText
          expected = "My karver is your karver."

      value `shouldBe` expected

    it "mix of object a identity #2" $ do
      let mixText  = "My {{ template.name }} is your {{ project }}."
          value    = renderer mixText
          expected = "My karver is your karver."

      value `shouldBe` expected

    it "array identity" $ do
      let arrText  = "karver uses {{ libraries[0] }} for parsing."
          value    = renderer arrText
          expected = "karver uses attoparsec for parsing."

      value `shouldBe` expected

    it "mix of array and identity" $ do
      let arrText  = "{{ project }} uses {{ libraries[1] }} for testing."
          value    = renderer arrText
          expected = "karver uses hspec for testing."

      value `shouldBe` expected

    it "mix of array and object" $ do
      let arrText  = append "{{ template.name }} uses"
                            " {{ libraries[1] }} for testing."
          value    = renderer arrText
          expected = "karver uses hspec for testing."

      value `shouldBe` expected
