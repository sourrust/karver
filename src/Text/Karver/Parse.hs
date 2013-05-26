{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.Parse
( literalParser
, identityParser
, objectParser
, arrayParser
) where

import Text.Karver.Types

import Data.Attoparsec.Text

literalParser :: Parser Tokens
literalParser = do
  html <- takeWhile1 (/= '{')
  return $ Literal html

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
    return $ Identity ident

objectParser :: Parser Tokens
objectParser =
  surroundParser $ do
    obj <- takeTill (inClass " .}")
    char '.'
    key <- takeTill (inClass " }")
    return $ Object obj key

arrayParser :: Parser Tokens
arrayParser =
  surroundParser $ do
    arr <- takeTill (inClass " [}")
    char '['
    idx <- decimal
    char ']'
    return $ Array arr idx
