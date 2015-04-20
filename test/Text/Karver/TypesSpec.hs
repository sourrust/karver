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
  describe "Coverting JSON to HashMap" $ do
    it "should decode a list with one Literal" $ do
      let json     = "[ \"Some text\" ]"
          value    = decode' json
          expected = Just . List $ V.fromList [Literal "Some text"]

      value `shouldBe` expected

    it "should decode a list with one Literal and one Object" $ do
      let json     = "[ \"Sample\", { \"key\": \"value\" } ]"
          value    = decode' json
          expected = Just . List $ V.fromList
                      [ Literal "Sample"
                      , Object $ H.fromList [ ("key", "value") ]
                      ]

      value `shouldBe` expected

    it "should decode an object with one key and value" $ do
      let json     = "{ \"name\": \"value\" }"
          value    = decode' json
          expected = Just . Object $ H.fromList [ ("name", "value") ]

      value `shouldBe` expected

    it "should decode an object with many keys and values" $ do
      let json     = "{ \"name\": \"one\", \"value\": \"two\""
                  <> ", \"key\": \"three\" }"
          value    = decode' json
          expected = Just . Object $ H.fromList
                       [ ("name", "one")
                       , ("value", "two")
                       , ("key", "three")
                       ]

      value `shouldBe` expected
