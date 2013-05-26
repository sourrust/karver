{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.Parse
( literalParser
, identityParser
, objectParser
, listParser
) where

import Text.Karver.Types

import Data.Attoparsec.Text

literalParser :: Parser Tokens
literalParser = do
  html <- takeWhile1 (/= '{')
  return $ LiteralTok html

surroundParser :: Parser Tokens -> Parser Tokens
surroundParser tokenParser = do
  string "{{"
  skipSpace
  tok <- tokenParser
  skipSpace
  string "}}"
  return tok

identityParser :: Parser Tokens
identityParser =
  surroundParser $ do
    ident <- takeTill (inClass " }")
    return $ IdentityTok ident

objectParser :: Parser Tokens
objectParser =
  surroundParser $ do
    obj <- takeTill (inClass " .}")
    char '.'
    key <- takeTill (inClass " }")
    return $ ObjectTok obj key

listParser :: Parser Tokens
listParser =
  surroundParser $ do
    list <- takeTill (inClass " [}")
    char '['
    idx <- decimal
    char ']'
    return $ ListTok list idx
