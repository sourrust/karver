module Text.Karver.Types where

import Data.Text (Text)
import Data.HashMap.Strict
import Data.Vector

type Key = Text

data Tokens = Literal  Text
            | Identity Text
            | Object   Text Key
            | Array    Text Int
            deriving (Show, Eq)

data Value = String Text
           | Map    (HashMap Text Text)
           | List   (Vector Text)
           deriving (Show)
