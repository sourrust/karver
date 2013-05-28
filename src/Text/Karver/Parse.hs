{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.Parse
( literalParser
, identityParser
, objectParser
, listParser
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

identityParser :: Parser Tokens
identityParser =
  identityDelimiter $ do
    ident <- takeTill (inClass " }")
    return $ IdentityTok ident

objectParser :: Parser Tokens
objectParser =
  identityDelimiter $ do
    obj <- takeTill (inClass " .}")
    char '.'
    key <- takeTill (inClass " }")
    return $ ObjectTok obj key

listParser :: Parser Tokens
listParser =
  identityDelimiter $ do
    list <- takeTill (inClass " [}")
    char '['
    idx <- decimal
    char ']'
    return $ ListTok list idx

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
