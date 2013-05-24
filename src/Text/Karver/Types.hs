module Text.Karver.Types where

import Data.Text (Text)

data Tokens = Literal  Text
            | Identity Text
            deriving (Show, Eq)
