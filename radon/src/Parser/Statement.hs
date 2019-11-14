module Parser.Statement where

import Control.Applicative hiding (some, many)

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Parser.Common
import Parser.Expression
import Parser.Type

pReturn :: Parser Stmt
pReturn = Return <$> (kReturn *> optional pExpr) <*> getNA <?> "return"

pDeclare :: Parser Stmt
pDeclare = do
  pos <- getNA
  quals <- many (kStatic <|> kInline <|> kConst)
  _ <- kVal
  name <- identifier
  _ <- colon
  typ <- pType
  _ <- equals
  value <- optional pExpr
  return (Declare name typ value pos) <?> "val"

pWhileSmall :: Parser Stmt
pWhileSmall = do
  pos <- getNA
  _ <- kWhile
  cond <- parens pExpr
  _ <- colon
  body <- pStmt
  return $ While cond [body] pos

pWhileFull :: Parser Stmt
pWhileFull = L.indentBlock scn preamb
 where 
  preamb = do
    pos <- getNA
    _ <- kWhile
    cond <- parens pExpr
    _ <- colon
    return $ L.IndentSome Nothing (\x -> return $ While cond x pos) pStmt

pWhile :: Parser Stmt
pWhile = pWhileSmall <|> pWhileFull <?> "while"

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

pFor :: Parser Stmt
pFor = pForSmall <|> pForFull

pStmtExpr :: Parser Stmt
pStmtExpr = SExpr <$> pExpr <*> getNA

pStmt :: Parser Stmt
pStmt = pReturn <|> pDeclare <|> pWhile <|> pFor <|> pStmtExpr