{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.Parse
( literalParser
, identityParser
, identityParser'
, objectParser
, objectParser'
, listParser
, listParser'
, conditionParser
) where

import Text.Karver.Types

import Data.Attoparsec.Text
import Data.Text (Text, empty, pack)

literalParser :: Parser Tokens
literalParser = do
  html <- takeWhile1 (/= '{')
  return $ LiteralTok html

delimiterParser :: Text -> Text -> Parser Tokens -> Parser Tokens
delimiterParser begin end tokenParser = do
  string begin
  skipSpace
  tok <- tokenParser
  skipSpace
  string end
  return tok

identityDelimiter, expressionDelimiter :: Parser Tokens -> Parser Tokens
identityDelimiter = delimiterParser "{{" "}}"

expressionDelimiter = delimiterParser "{%" "%}"

identityParser_ :: (Parser Tokens -> Parser Tokens) -> Parser Tokens
identityParser_ fn = fn $ do
  ident <- takeTill (inClass " }")
  return $ IdentityTok ident

identityParser, identityParser' :: Parser Tokens

identityParser  = identityParser_ identityDelimiter
identityParser' = identityParser_ id

objectParser_ :: (Parser Tokens -> Parser Tokens) -> Parser Tokens
objectParser_ fn = fn $ do
  obj <- takeTill (inClass " .}")
  char '.'
  key <- takeTill (inClass " }")
  return $ ObjectTok obj key

objectParser, objectParser' :: Parser Tokens

objectParser  = objectParser_ identityDelimiter
objectParser' = objectParser_ id

listParser_ :: (Parser Tokens -> Parser Tokens) -> Parser Tokens
listParser_ fn = fn $ do
  list <- takeTill (inClass " [}")
  char '['
  idx <- decimal
  char ']'
  return $ ListTok list idx

listParser, listParser' :: Parser Tokens

listParser  = listParser_ identityDelimiter
listParser' = listParser_ id

conditionParser :: Parser Tokens
conditionParser = do
  (LiteralTok logic) <- expressionDelimiter $ do
    string "if"
    skipSpace
    condition <- takeTill (inClass " %")
    return $ LiteralTok condition
  skipSpace
  ifbody <- manyTill anyChar (try $ string "{% endif %}")
  return $ ConditionTok logic (pack ifbody) empty
