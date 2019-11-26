-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.Statement
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a parsing utilities for statements
-----------------------------------------------------------------------------
module Parser.Statement where

import Control.Applicative hiding (some, many)

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Parser.Common
import Parser.Expression
import Parser.Type

-- | Parses a return statement
--
-- > return 1
pReturn :: Parser Stmt
pReturn = Return <$> (kReturn *> optional pExpr) <*> getNA <?> "return"

-- | Parses a val declaration
--
-- > val x: String = "hi"
pDeclare :: Parser Stmt
pDeclare = do
  pos <- getNA
  quals <- many pVarQual
  _ <- kVal
  name <- identifier
  _ <- colon
  typ <- pType
  value <- optional (equals *> pExpr)
  return (Declare name (foldr ($) typ quals) value pos) <?> "val"

-- | Parses a while statement
--
-- > while(1): statement
-- or
--
-- > while(1):
-- > statement1
-- > statement2
-- > ..
pWhile :: Parser Stmt
pWhile = pWhileSmall <|> pWhileFull <?> "while"
 where
  pWhileSmall :: Parser Stmt
  pWhileSmall = do
    (pos, cond) <- pWhileStart
    body <- pStmt
    return $ While cond [body] pos

  pWhileFull :: Parser Stmt
  pWhileFull = L.indentBlock scn preamb
   where
    preamb = do
      (pos, cond) <- pWhileStart
      return $ L.IndentSome Nothing (\x -> return $ While cond x pos) pStmt

  pWhileStart :: Parser (NodeAnnotation, Expr)
  pWhileStart = do
    pos <- getNA
    _ <- kWhile
    cond <- parens pExpr
    _ <- colon
    return (pos, cond)

-- if (x): 1
--
-- if (x): 1
-- elif (x): 2
-- else: 3
--
-- pIf :: Parser Stmt
-- pIf = do
--   _ <- kIf
--   expr <- parens pExpr
--   _ <- colon

-- | parses a for statement
--
-- > for (val x: Int = 0; x < 10; x++): statement
-- or
--
-- > for (val x: Int = 0; x < 10; x++):
-- > statement1
-- > statement2
-- > ..
pFor :: Parser Stmt
pFor = (pForSmall <|> pForFull) <?> "for"
 where
  pForSmall :: Parser Stmt
  pForSmall = do
    (pos, initial, cond, fin) <- pForPreamb
    body <- pStmt
    return $ For initial cond fin [body] pos

  pForFull :: Parser Stmt
  pForFull = L.indentBlock scn preamb
   where
    preamb = do
      (pos, initial, cond, fin) <- pForPreamb
      return $ L.IndentSome Nothing (\x -> return $ For initial cond fin x pos) pStmt

  pForPreamb :: Parser (NodeAnnotation, Stmt, Expr, Expr)
  pForPreamb = do
    pos <- getNA
    _ <- kFor
    _ <- lparen
    initial <- pStmt
    _ <- semi
    cond <- pExpr
    _ <- semi
    fin  <- pExpr
    _ <- rparen
    _ <- colon
    return (pos, initial, cond, fin)

-- | Parses expressions as a statement throwing away the side effect
pStmtExpr :: Parser Stmt
pStmtExpr = SExpr <$> pExpr <*> getNA

-- | Tries parsing any of the statements
pStmt :: Parser Stmt
pStmt = pReturn <|> pDeclare <|> try pWhile <|> try pFor <|> pStmtExpr
