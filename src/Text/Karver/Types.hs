-- |
-- Module:      Data.Karver.Types
-- Copyright:   Jeremy Hull 2013
-- License:     BSD3
--
-- Maintainer:  Jeremy Hull <sourdrums@gmail.com>
-- Stability:   experimental
-- Portability: unknown
--
-- Base types used throughout Karver.

module Text.Karver.Types
( Tokens(..)
, Value(..)
) where

import Prelude hiding (FilePath)

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

type Element    = Text
type LoopBody   = Text

type FilePath   = Text

-- | When dealing with the syntax of karver, we first traslate the given
-- 'Text' in 'Tokens' for easier manipulation. Each 'Tokens' type is
-- a repesentation of a certain type of data.
data Tokens = LiteralTok   Text
            | IdentityTok  Text
            | ObjectTok    ObjectName Key
            | ListTok      ListName   Index
            | ConditionTok Compare    IfBody  ElseBody
            | LoopTok      ListName   Element LoopBody
            | IncludeTok   FilePath
            deriving (Show, Eq)

-- | Fairly basic work around for using different types inside a 'HashMap'.
-- The 'Value' type also make it possible for 'List' to contain more than
-- one type.
data Value = Literal Text
           | Object (HashMap Text Text)
           | List   (Vector Value)
           deriving (Show)
