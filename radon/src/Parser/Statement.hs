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
import AST.Phases.Parsed

import Parser.Common
import Parser.Expression
import Parser.Type

-- | Parses a return statement
--
-- > return 1
pReturn :: Parser StmtPA
pReturn = ReturnPA <$> getNS <*> (kReturn *> optional pExpr) <?> "return"

-- | Parses a val declaration
--
-- > val x: String = "hi"
pDeclare :: Parser StmtPA
pDeclare = do
  pos <- getNS
  quals <- many pVarQual
  _ <- kVal
  name <- identifier
  _ <- colon
  typ <- optional pType
  value <- optional (equals *> pExpr)
  pure (DeclarePA pos name (foldr ($) typ quals) value) <?> "val"

-- | Parses a while statement
--
-- > while(1): statement
-- or
--
-- > while(1):
-- > statement1
-- > statement2
-- > ..
pWhile :: Parser StmtPA
pWhile = pWhileSmall <|> pWhileFull <?> "while"
 where
  pWhileSmall :: Parser StmtPA
  pWhileSmall = do
    (pos, cond) <- pWhileStart
    body <- pStmt
    pure $ WhilePA pos cond [body]

  pWhileFull :: Parser StmtPA
  pWhileFull = L.indentBlock scn preamb
   where
    preamb = do
      (pos, cond) <- pWhileStart
      pure $ L.IndentSome Nothing (pure . WhilePA pos cond) pStmt

  pWhileStart :: Parser (NodeSource, ExprPA)
  pWhileStart = do
    pos <- getNS
    _ <- kWhile
    cond <- parens pExpr
    _ <- colon
    pure (pos, cond)

-- if x: 1
--
-- if x: 1
-- elif x: 2
-- else: 3
--
-- pIf :: Parser Stmt
-- pIf = do
--   _ <- kIf
--   expr <- pExpr
--   _ <- colon

-- | parses a for statement
--
-- > for (val x: Int = 0; x < 10; x++): statement
-- or
--
-- > for (val x: Int = 0; x < 10; x++):
-- >  statement1
-- >  statement2
-- > ..
pFor :: Parser StmtPA
pFor = (pForSmall <|> pForFull) <?> "for"
 where
  pForSmall :: Parser StmtPA
  pForSmall = do
    (pos, initial, cond, fin) <- pForPreamb
    body <- pStmt
    pure $ ForPA pos initial cond fin [body]

  pForFull :: Parser StmtPA
  pForFull = L.indentBlock scn preamb
   where
    preamb = do
      (pos, initial, cond, fin) <- pForPreamb
      pure $ L.IndentSome Nothing (pure . ForPA pos initial cond fin) pStmt

  pForPreamb :: Parser (NodeSource, StmtPA, ExprPA, ExprPA)
  pForPreamb = do
    pos <- getNS
    _ <- kFor
    _ <- lparen
    initial <- pStmt
    _ <- semi
    cond <- pExpr
    _ <- semi
    fin  <- pExpr
    _ <- rparen
    _ <- colon
    pure (pos, initial, cond, fin)

-- | Parses expressions as a statement throwing away the side effect
pStmtExpr :: Parser StmtPA
pStmtExpr = SExprPA <$> getNS <*> pExpr

-- | Tries parsing any of the statements
pStmt :: Parser StmtPA
pStmt = pReturn <|> pDeclare <|> try pWhile <|> try pFor <|> pStmtExpr
