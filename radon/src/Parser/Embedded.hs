{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.Expression
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides parsing support for other languages embedded
-----------------------------------------------------------------------------
module Parser.Embedded where

import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char (asciiChar)

import AST
import Parser.Common

embeddedBlock :: Parser (Text, Language)
embeddedBlock = do
  lang <- bracks language
  body <- T.pack <$> (lbraceper *> manyTill asciiChar rbraceper)
  return (body, lang)

pExprEmbed :: Parser (Embedded 'EExpr)
pExprEmbed = uncurry EmbeddedExpr <$> embeddedBlock

pStmtEmbed :: Parser (Embedded 'EStmt)
pStmtEmbed = uncurry EmbeddedStmt <$> embeddedBlock

pTypeEmbed :: Parser (Embedded 'EType)
pTypeEmbed = uncurry EmbeddedType <$> embeddedBlock

pLitEmbed :: Parser (Embedded 'ELit)
pLitEmbed = uncurry EmbeddedLit <$> embeddedBlock
