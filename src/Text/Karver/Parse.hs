{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.Parse
( literalParser
, variableParser
, variableParser'
, conditionParser
) where

import Text.Karver.Types

import Data.Attoparsec.Text
import Data.Text (Text, empty, pack)

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
identityDelimiter = delimiterParser "{{" "}}"

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
