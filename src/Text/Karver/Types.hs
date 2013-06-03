module Text.Karver.Types where

import Data.Text (Text)
import Data.HashMap.Strict
import Data.Vector

type ObjectName = Text
type Key        = Text

data Tokens = LiteralTok   Text
            | IdentityTok  Text
            | ObjectTok    ObjectName Key
            | ListTok      Text Int
            | ConditionTok Text Text Text
            | LoopTok      Text Text Text
            deriving (Show, Eq)

data Value = Literal Text
           | Object (HashMap Text Text)
           | List   (Vector Text)
           deriving (Show)
