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

data Tokens = LiteralTok   Text
            | IdentityTok  Text
            | ObjectTok    ObjectName Key
            | ListTok      ListName   Index
            | ConditionTok Compare    IfBody  ElseBody
            | LoopTok      ListName   Element LoopBody
            | IncludeTok   FilePath
            deriving (Show, Eq)

data Value = Literal Text
           | Object (HashMap Text Text)
           | List   (Vector Value)
           deriving (Show)
