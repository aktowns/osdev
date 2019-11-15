-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.Expression
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a parsing utilities for expressions
-----------------------------------------------------------------------------
module Parser.Expression where

import Data.Functor ((<&>))
import Control.Applicative hiding (some, many)
import Control.Monad.Combinators.Expr

import Text.Megaparsec
import Text.Megaparsec.Char (char)

import AST
import Parser.Common
import Parser.Type

table :: [[Operator Parser Expr]]
table = [ [ prefix  minusminus   $ Unary  UnaryPrefix  Decrement
          , prefix  plusplus     $ Unary  UnaryPrefix  Increment
          , postfix minusminus   $ Unary  UnaryPostfix Decrement
          , postfix plusplus     $ Unary  UnaryPostfix Increment ]
        , [ prefix  (sing minus) $ Unary  UnaryPrefix  Negate
          , prefix  (sing plus)  $ Unary  UnaryPrefix  Positive ]
        , [ binary  star         $ Binary Mul
          , binary  fslash       $ Binary Div ]
        , [ binary  plus         $ Binary Add
          , binary  minus        $ Binary Sub ]
        , [ binary  shiftl       $ Binary ShiftLeft
          , binary  shiftr       $ Binary ShiftRight ]
        , [ binary  langle       $ Binary LessThan
          , binary  langleeq     $ Binary LessThanEqual
          , binary  rangle       $ Binary GreaterThan
          , binary  rangleeq     $ Binary GreaterThanEqual ]
        , [ binary  eqeq         $ Binary Equals
          , binary  neq          $ Binary NotEquals ]
        , [ binary  amp          $ Binary BitwiseAnd ]
        , [ binary  caret        $ Binary BitwiseXor ]
        , [ binary  pipe         $ Binary BitwiseOr ]
        , [ binary  ampamp       $ Binary LogicalAnd ]
        , [ binary  pipepipe     $ Binary LogicalOr ]
        ]
 where
  binary :: Parser b -> (a -> a -> NodeAnnotation -> a) -> Operator Parser a
  binary  name f = InfixL $ (getNA <&> niflip3 f) <* name
   where
    niflip3 :: (a -> b -> c -> d) -> c -> a -> b -> d
    niflip3 f' e3 e1 e2 = f' e1 e2 e3

  prefix :: Parser b -> (a -> NodeAnnotation -> a) -> Operator Parser a
  prefix  name f = Prefix $ (getNA <&> flip f) <* name

  sing :: Parser a -> Parser a
  sing n = try (n <* notFollowedBy n)

  postfix :: Parser b -> (a -> NodeAnnotation -> a) -> Operator Parser a
  postfix name f = Postfix $ (getNA <&> flip f) <* name

pInteger :: Parser Expr
pInteger = try pHex <|> try pOct <|> pInt
 where
  intTy :: Parser [IntType]
  intTy = many (
           try (Unsigned <$ char 'u')
       <|> try (LongLong <$ (char 'l' >> char 'l'))
       <|> (Long <$ char 'l'))

  pHex :: Parser Expr
  pHex = do
    p <- getNA
    _ <- char '0' >> char 'x'
    i <- hexadecimal
    t <- intTy
    return $ Literal (IntLiteral i Hex t) p

  pOct :: Parser Expr
  pOct = do
    p <- getNA
    _ <- char '0'
    i <- octal
    t <- intTy
    return $ Literal (IntLiteral i Oct t) p

  pInt :: Parser Expr
  pInt = do
    p <- getNA
    i <- integer
    t <- intTy
    return $ Literal (IntLiteral i Dec t) p


pString :: Parser Expr
pString = Literal . StrLiteral <$> stringLiteral' <*> getNA

pChar :: Parser Expr
pChar = Literal . CharLiteral <$> charLiteral <*> getNA

pFuncCall :: Parser Expr
pFuncCall = do
  pos <- getNA
  name <- identifier
  args <- parens $ pExpr `sepBy` comma
  return $ FunCall name args pos

pTerm :: Parser Expr
pTerm = pString
    <|> pInteger
    <|> pChar
    <|> pCast
    <|> try pAssign
    <|> try pFuncCall
    <|> try pArraySub
    <|> pIdentifier
    <|> pCIdentifier

pCast :: Parser Expr
pCast = do
  pos <- getNA
  typ <- parens pType
  expr <- pExpr
  return $ Cast typ expr pos

pIdentifier :: Parser Expr
pIdentifier = Identifier <$> identifier <*> getNA

pCIdentifier :: Parser Expr
pCIdentifier = Identifier <$> cIdentifier <*> getNA

pArraySub :: Parser Expr
pArraySub = do
  pos <- getNA
  ident <- identifier <|> cIdentifier
  subsc <- bracks pExpr
  return $ ArraySub ident subsc pos

pAssign :: Parser Expr
pAssign = do
  pos <- getNA
  left <- try pArraySub <|> pIdentifier <|> pCIdentifier
  _ <- equals
  right <- pExpr
  return $ Assign left right pos

pExpr :: Parser Expr
pExpr = makeExprParser pTerm table
