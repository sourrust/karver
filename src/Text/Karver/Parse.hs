-- |
-- Module:      Data.Karver.Parse
-- Copyright:   Jeremy Hull 2015
-- License:     BSD3
--
-- Maintainer:  Jeremy Hull <sourdrums@gmail.com>
-- Stability:   experimental
-- Portability: unknown
--
-- All the 'Parser's are defined here, including the one used by the top
-- level module "Text.Karver".

{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.Parse
( templateParser
, literalParser
, variableParser
, variableParser'
, conditionParser
, loopParser
, includeParser
) where

import Prelude hiding (take)

import Text.Karver.Types

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator (lookAhead)
import Data.Monoid ((<>))
import Data.Text (Text, empty, pack)
import Control.Applicative ((<|>), (<$>), (*>), (<*))

-- | Top level 'Parser' that will translate 'Text' into ['Token']
templateParser :: Parser [Token]
templateParser = many1 $ choice [ variableParser
                                , conditionParser
                                , loopParser
                                , literalParser
                                , includeParser
                                ]


-- | Takes everything until it reaches a @{@, resulting in the 'LiteralTok'
literalParser :: Parser Token
literalParser = LiteralTok <$> _literalParser
 where
  _literalParser = do
    html  <- takeWhile1 (/= '{')
    isEnd <- atEnd
    if isEnd
      then return html
      else _continueParsing html

  _continueParsing html = do
    peek <- lookAhead $ take 2 <|> take 1
    case peek of
      "{{" -> return html
      "{%" -> return html
      "{"  -> (html <>) <$> take 1
      _    -> do
        currentText <- (html <>) <$> take 1
        (currentText  <>) <$> _literalParser

-- General function for making parsers that will be surrounded by a curtain
-- delimiter — which has both a beginning and end.
delimiterParser :: Text -> Text -> Parser a -> Parser a
delimiterParser begin end parseFunc = do
  string begin
  skipSpace
  val <- parseFunc
  skipSpace
  string end
  return val

identityDelimiter, expressionDelimiter :: Parser a -> Parser a

identityDelimiter   = delimiterParser "{{" "}}"
expressionDelimiter = delimiterParser "{%" "%}"

-- General parser for the several variable types. It is basically used to
-- not repeat parsers with and without a delimiter.
variableParser_ :: (Parser Token -> Parser Token) -> Parser Token
variableParser_ fn = fn $ do
  ident <- takeTill (inClass " .[}")
  peek <- peekChar
  case peek of
    (Just '[') -> do
      char '['
      idx <- decimal
      char ']'
      return $ ListTok ident idx
    (Just '.') -> do
      char '.'
      key <- takeTill (inClass " }")
      return $ ObjectTok ident key
    (Just ' ') -> return $ IdentityTok ident
    (Just '}') -> return $ IdentityTok ident
    Nothing    -> return $ IdentityTok ident
    _          -> fail "variableParser_: failed with no token to apply."

variableParser, variableParser' :: Parser Token

-- | 'Parser' for all the variable types. Returning on of the following
-- 'Token's:
--
-- * 'IncludeTok'
--
-- * 'ListTok'
--
-- * 'ObjectTok'
variableParser  = variableParser_ identityDelimiter

-- | 'Parser' for all the variable types. Returning on of the following
-- 'Token's:
--
-- * 'IncludeTok'
--
-- * 'ListTok'
--
-- * 'ObjectTok'
--
-- This is without the delimiter
variableParser' = variableParser_ id

-- Parser for skipping over horizontal space and end on a newline
-- character, which will be skipped as well.
skipSpaceTillEOL :: Parser ()
skipSpaceTillEOL = option () $ skipWhile isHorizontalSpace >> endOfLine
{-# INLINE skipSpaceTillEOL #-}

-- | 'Parser' for if statements, that will result in the 'ConditionTok'
conditionParser :: Parser Token
conditionParser = do
  logic <- expressionDelimiter $ do
    string "if"
    skipSpace
    condition <- takeTill (inClass " %")
    return condition
  let anyTill   = manyTill anyChar
      ifparse   = skipSpaceTillEOL *> anyTill (expressionDelimiter
                                             $ string "endif"
                                           <|> string "else")
      elseparse = skipSpaceTillEOL *> anyTill (expressionDelimiter
                                             $ string "endif")
  ifbody <- pack <$> ifparse
  elsebody <- option empty (pack <$> elseparse)
  skipSpaceTillEOL
  return $ ConditionTok logic ifbody elsebody

-- | 'Parser' for for loops, that will result in the 'LoopTok'
loopParser :: Parser Token
loopParser = do
  (arr, var) <- expressionDelimiter $ do
    string "for"
    skipSpace
    varName <- takeTill (== ' ')
    skipSpace
    string "in"
    skipSpace
    arrName <- takeTill (inClass " %")
    return (arrName, varName)
  skipSpaceTillEOL
  loopbody <- manyTill anyChar (expressionDelimiter $ string "endfor")
  skipSpaceTillEOL
  return $ LoopTok arr var $ pack loopbody

-- | 'Parser' for includes, that will result in 'IncludeTok'
includeParser :: Parser Token
includeParser = expressionDelimiter $ do
  let quote c = char c *> takeTill (== c) <* char c
  string "include"
  skipSpace
  filepath <- quote '"' <|> quote '\''
  return $ IncludeTok filepath
