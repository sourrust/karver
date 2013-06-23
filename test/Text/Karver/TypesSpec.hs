{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.TypesSpec (spec) where

import Text.Karver.Types

import Data.Aeson (decode')
import qualified Data.HashMap.Strict as H
import Data.Monoid ((<>))
import qualified Data.Vector as V
import Test.Hspec

spec :: Spec
spec = do
  describe "Coverting from JSON" $ do
    it "List with 1 Literal" $ do
      let json     = "[ \"Some text\" ]"
          value    = decode' json
          expected = Just . List $ V.fromList [Literal "Some text"]

      value `shouldBe` expected

    it "List with 1 Literal and 1 Object" $ do
      let json     = "[ \"Sample\", { \"key\": \"value\" } ]"
          value    = decode' json
          expected = Just . List $ V.fromList
                      [ Literal "Sample"
                      , Object $ H.fromList [ ("key", "value") ]
                      ]

      value `shouldBe` expected

    it "Object with one key value" $ do
      let json     = "{ \"name\": \"value\" }"
          value    = decode' json
          expected = Just . Object $ H.fromList [ ("name", "value") ]

      value `shouldBe` expected

    it "Object with many key value" $ do
      let json     = "{ \"name\": \"one\", \"value\": \"two\""
                  <> ", \"key\": \"three\" }"
          value    = decode' json
          expected = Just . Object $ H.fromList
                       [ ("name", "one")
                       , ("value", "two")
                       , ("key", "three")
                       ]

      value `shouldBe` expected
