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

import Data.Text (Text)
import Data.HashMap.Strict
import Data.Vector

-- | When dealing with the syntax of karver, we first translate the given
-- 'Text' in 'Tokens' for easier manipulation. Each 'Tokens' type is
-- a representation of a certain type of data.
data Tokens = LiteralTok   Text
            | IdentityTok  Text
            | ObjectTok    Text Text
            | ListTok      Text Int
            | ConditionTok Text Text Text
            | LoopTok      Text Text Text
            | IncludeTok   Text
            deriving (Show, Eq)

-- | Fairly basic work around for using different types inside a 'HashMap'.
-- The 'Value' type also make it possible for 'List' to contain more than
-- one type.
data Value = Literal Text
           | Object (HashMap Text Text)
           | List   (Vector Value)
           deriving (Show)
