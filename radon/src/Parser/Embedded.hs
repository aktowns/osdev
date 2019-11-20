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

import Data.Functor ((<&>))
import Control.Applicative hiding (some, many)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char (asciiChar, space, char)

import AST
import Parser.Common

uncurry2 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry2 f (a,b,c) = f a b c

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
