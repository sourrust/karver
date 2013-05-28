module Text.Karver.Types where

import Data.Text (Text)
import Data.HashMap.Strict
import Data.Vector

type Key = Text

data Tokens = LiteralTok   Text
            | IdentityTok  Text
            | ObjectTok    Text Key
            | ListTok      Text Int
            | ConditionTok Text Text Text
            deriving (Show, Eq)

data Value = Literal Text
           | Object (HashMap Text Text)
           | List   (Vector Text)
           deriving (Show)
