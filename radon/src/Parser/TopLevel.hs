-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.TopLevel
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a parsing utilities for top level statements
-----------------------------------------------------------------------------
module Parser.TopLevel where

import Data.Text (Text)
import Control.Applicative hiding (some, many)
import Data.Maybe (fromMaybe)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Parser.Common
import Parser.Expression
import Parser.Type
import Parser.Statement

pEnum :: Parser TL
pEnum = L.indentBlock scn preamb
 where
  preamb = do
    pos <- getNA
    _ <- kEnum
    name <- cIdentifier
    _ <- equals
    return $ L.IndentSome Nothing (\x -> return $ Enum name x pos) pEnumBody

  pEnumBody :: Parser (Text, Maybe Integer)
  pEnumBody = (,) <$> cIdentifier <*> optional (equals *> integer)

pFunc :: Parser TL
pFunc = dbg "func" $ (try pFuncSmall) <|> pFuncFull
 where
  pFuncSmall :: Parser TL
  pFuncSmall = do
    (pos, quals, name, args, typ) <- pFuncPreamb
    body <- pStmt
    return $ Func name (foldr ($) typ quals) args [body] pos

  pFuncFull :: Parser TL
  pFuncFull = L.nonIndented scn (L.indentBlock scn preamb)
   where
    preamb = do
      (pos, quals, name, args, typ) <- pFuncPreamb
      return $ L.IndentSome Nothing (\x -> return $ Func name (foldr ($) typ quals) args x pos) pStmt

  pFuncPreamb :: Parser (NodeAnnotation, [Type -> Type], Text, [(Text, Type)], Type)
  pFuncPreamb = do
      pos <- getNA
      quals <- many pFunQual
      name <- identifier
      args <- optional $ parens pArgs
      _ <- colon
      typ <- pType
      _ <- equals
      return (pos, quals, name, fromMaybe [] args, typ)
   where
    pArgs :: Parser [(Text, Type)]
    pArgs = ((,) <$> (identifier <* colon) <*> pType) `sepBy` comma

pDecl :: Parser TL
pDecl = do
  pos <- getNA
  quals <- many pVarQual
  name <- kVal *> cIdentifier <* colon
  typ <- pType
  value <- optional $ equals *> pExpr
  return (Decl name (foldr ($) typ quals) value pos) <?> "val"

pModule :: Parser TL
pModule = dbg "?" $ L.nonIndented scn (L.indentBlock scn preamb)
 where
  preamb = do
    pos <- getNA
    name <- kModule *> cIdentifier <* equals
    return $ L.IndentSome Nothing (\x -> return $ Module name x pos) pTopLevelEntry

pTopLevelEntry :: Parser TL
pTopLevelEntry = try pEnum <|> try pFunc <|> try pDecl <|> try pModule

pTopLevel :: Parser [TL]
pTopLevel = L.nonIndented scn $ many (pTopLevelEntry <* space)

