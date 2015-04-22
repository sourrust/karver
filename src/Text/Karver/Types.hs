-- |
-- Module:      Data.Karver.Types
-- Copyright:   Jeremy Hull 2015
-- License:     BSD3
--
-- Maintainer:  Jeremy Hull <sourdrums@gmail.com>
-- Stability:   experimental
-- Portability: unknown
--
-- Base types used throughout Karver.

module Text.Karver.Types
( Token(..)
, Value(..)
) where

import Control.Applicative ((<$>))
import qualified Data.Aeson as A
import Data.Text (Text)
import Data.HashMap.Strict
import Data.Vector

-- | When dealing with the syntax of karver, we first translate the given
-- 'Text' into 'Token's for easier manipulation. Each 'Token' type is
-- a representation of a certain type of data.
data Token = LiteralTok   Text
           -- ^ Literal token. This is the default 'Token' that gets
           --   matched only if it isn't any of the others.
           | IdentityTok  Text
           -- ^ Identity token. This is for a regular variable with no
           --   sign of it being an object or list. eg. {{ ident }}
           | ObjectTok    Text Text
           -- ^ Object token. This is similar to 'IdentityTok', but if
           --   there is a dot, it gets placed in the second value. The
           --   first 'Text' is the object name, while the second 'Text'
           --   is the key to the object. eg. {{ ident.key }}
           | ListTok      Text Int
           -- ^ List token. This is also similar to the 'IdentityTok', but
           --   if there is an opening square bracket, it gets place in
           --   the second value. 'Text' is the list name, while 'Int' is
           --   the index. eg {{ ident[4] }}
           | ConditionTok Text Text Text
           -- ^ If statement token. The first 'Text' will be the check if
           --   a identity is available or not. Second 'Text' is the body
           --   of the if statement. And the third 'Text' is the else body
           --   — if their isn't one, it will be empty.
           | LoopTok      Text Text Text
           -- ^ For loop token. The first 'Text' is the list that will be
           --   iterated over. Second 'Text' is the variable name a single
           --   element of the list will be placed into. Third 'Text' is
           --   the body of the loop that will be repeatedly translated
           --   from.
           | IncludeTok   Text
           -- ^ Include token. The 'Text' value store a file name, which
           --   includes its relative path, based on the current working
           --   directory.
           deriving (Show, Eq)

-- | Fairly basic work around for using different types inside a 'HashMap'.
-- The 'Value' type also make it possible for 'List' to contain more than
-- one type.
data Value = Literal Text
           -- ^ The base value for the storing of variable.
           | Object (HashMap Text Text)
           -- ^ An alias for 'HashMap', that will only hold 'Text' with
           --   'Text' as a key as well.
           | List   (Vector Value)
           -- ^ An alias for 'Vector', that can hold all three 'Value's
           --   — which isn't desirable, because their can be nested
           --   'List's.
           deriving (Show, Eq)

instance A.FromJSON Value where
  parseJSON o@(A.Object _) = Object <$> A.parseJSON o
  parseJSON a@(A.Array _)  = List <$> A.parseJSON a
  parseJSON v              = Literal <$> A.parseJSON v
  {-# INLINE parseJSON #-}
