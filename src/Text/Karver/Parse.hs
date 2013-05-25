{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.Parse where

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
identityParser = do
  string "{{"
  skipSpace
  ident <- takeTill (inClass " }")
  skipSpace
  string "}}"
  return $ Identity ident

objectParser :: Parser Tokens
objectParser = do
  string "{{"
  skipSpace
  obj <- takeTill (inClass " .}")
  char '.'
  key <- takeTill (inClass " }")
  skipSpace
  string "}}"
  return $ Object obj key
