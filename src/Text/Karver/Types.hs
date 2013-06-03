module Text.Karver.Types where

import Data.Text (Text)
import Data.HashMap.Strict
import Data.Vector

type ObjectName = Text
type Key        = Text

type ListName   = Text
type Index      = Int

type Compare    = Text
type IfBody     = Text
type ElseBody   = Text

data Tokens = LiteralTok   Text
            | IdentityTok  Text
            | ObjectTok    ObjectName Key
            | ListTok      ListName   Index
            | ConditionTok Compare    IfBody  ElseBody
            | LoopTok      Text Text Text
            deriving (Show, Eq)

data Value = Literal Text
           | Object (HashMap Text Text)
           | List   (Vector Text)
           deriving (Show)
