module Text.Karver.Types where

import Data.Text (Text)
import Data.HashMap.Strict

type Key = Text

data Tokens = Literal  Text
            | Identity Text
            | Object   Text Key
            deriving (Show, Eq)

data Value = String Text
           | Map    (HashMap Text Text)
           deriving (Show)
