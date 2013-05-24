module Text.Karver.Types where

import Data.Text (Text)

type Key = Text

data Tokens = Literal  Text
            | Identity Text
            | Object   Text Key
            deriving (Show, Eq)
