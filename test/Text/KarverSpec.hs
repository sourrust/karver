{-# LANGUAGE OverloadedStrings #-}

module Text.KarverSpec (spec) where

import Text.Karver
import Text.Karver.Types

import Prelude hiding (unlines)
import Data.HashMap.Strict (fromList)
import Data.Text (Text, append, unlines)
import Test.Hspec

renderer :: Text -> Text
renderer = renderTemplate
  (fromList $ [ ("project",     String "karver")
              , ("language",    String "haskell")
              , ("ver-control", String "git")
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
