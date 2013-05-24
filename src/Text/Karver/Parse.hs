{-# LANGUAGE OverloadedStrings #-}

module Text.Karver.Parse where

import Text.Karver.Types

import Data.Attoparsec.Text

literalParser :: Parser Tokens
literalParser = do
  html <- takeWhile1 (/= '{')
  return $ Literal html

identityParser :: Parser Tokens
identityParser = do
  string "{{"
  skipSpace
  ident <- takeTill (inClass " }")
  skipSpace
  string "}}"
  return $! Identity ident
