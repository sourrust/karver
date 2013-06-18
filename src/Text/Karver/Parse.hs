-- |
-- Module:      Data.Karver.Parse
-- Copyright:   Jeremy Hull 2013
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

import Text.Karver.Types

import Data.Attoparsec.Text
import Data.Text (Text, empty, pack)
import Control.Applicative ((<|>), (<$>), (*>), (<*))

templateParser :: Parser [Tokens]
templateParser = many1 $ choice [ variableParser
                                , conditionParser
                                , loopParser
                                , literalParser
                                , includeParser
                                ]


literalParser :: Parser Tokens
literalParser = do
  html <- takeWhile1 (/= '{')
  return $ LiteralTok html

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

variableParser_ :: (Parser Tokens -> Parser Tokens) -> Parser Tokens
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

variableParser, variableParser' :: Parser Tokens

variableParser  = variableParser_ identityDelimiter
variableParser' = variableParser_ id

skipSpaceTillEOL :: Parser ()
skipSpaceTillEOL = option () $ skipWhile isHorizontalSpace >> endOfLine
{-# INLINE skipSpaceTillEOL #-}

conditionParser :: Parser Tokens
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

loopParser :: Parser Tokens
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

includeParser :: Parser Tokens
includeParser = expressionDelimiter $ do
  let quote c = char c *> takeTill (== c) <* char c
  string "include"
  skipSpace
  filepath <- quote '"' <|> quote '\''
  return $ IncludeTok filepath
