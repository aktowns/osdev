{-# LANGUAGE LambdaCase #-}
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

import Control.Applicative hiding (some, many)
import Control.Monad.Combinators.Expr

import Text.Megaparsec
import Text.Megaparsec.Char (char)

import AST
import AST.Phases.Parsed
import Parser.Common
import Parser.Type

table :: [[Operator Parser ExprPA]]
table = [ [ binary  dot          $ \p -> MemberRef p FieldMem
          , binary  larrow       $ \p -> MemberRef p PtrMem
          , binary  coloncolon   $ \p -> MemberRef p ModMem ]
        , [ prefix  minusminus   $ \p -> Unary p UnaryPrefix  Decrement
          , prefix  plusplus     $ \p -> Unary p UnaryPrefix  Increment
          , postfix minusminus   $ \p -> Unary p UnaryPostfix Decrement
          , postfix plusplus     $ \p -> Unary p UnaryPostfix Increment ]
        , [ prefix  (sing minus) $ \p -> Unary p UnaryPrefix  Negate
          , prefix  (sing plus)  $ \p -> Unary p UnaryPrefix  Positive ]
        , [ binary  star         $ \p -> Binary p Mul
          , binary  fslash       $ \p -> Binary p Div ]
        , [ binary  plus         $ \p -> Binary p Add
          , binary  minus        $ \p -> Binary p Sub ]
        , [ binary  shiftl       $ \p -> Binary p ShiftLeft
          , binary  shiftr       $ \p -> Binary p ShiftRight ]
        , [ binary  langle       $ \p -> Binary p LessThan
          , binary  langleeq     $ \p -> Binary p LessThanEqual
          , binary  rangle       $ \p -> Binary p GreaterThan
          , binary  rangleeq     $ \p -> Binary p GreaterThanEqual ]
        , [ binary  eqeq         $ \p -> Binary p Equals
          , binary  neq          $ \p -> Binary p NotEquals ]
        , [ binary  amp          $ \p -> Binary p BitwiseAnd ]
        , [ binary  caret        $ \p -> Binary p BitwiseXor ]
        , [ binary  pipe         $ \p -> Binary p BitwiseOr ]
        , [ binary  ampamp       $ \p -> Binary p LogicalAnd ]
        , [ binary  pipepipe     $ \p -> Binary p LogicalOr ]
        ]
 where
  binary :: Parser b -> (NodeSource -> a -> a -> a) -> Operator Parser a
  binary name f = InfixL $ (getNS <&> f) <* name

  prefix :: Parser b -> (NodeSource -> a -> a) -> Operator Parser a
  prefix name f = Prefix $ (getNS <&> f) <* name

  sing :: Parser a -> Parser a
  sing n = try (n <* notFollowedBy n)

  postfix :: Parser b -> (NodeSource -> a -> a) -> Operator Parser a
  postfix name f = Postfix $ (getNS <&> f) <* name

pInteger :: Parser ExprPA
pInteger = try pHex <|> try pOct <|> pInt
 where
  intTy :: Parser [IntType]
  intTy = many (
           try (Unsigned <$ char 'u')
       <|> try (LongLong <$ (char 'l' >> char 'l'))
       <|> (Long <$ char 'l'))

  pHex :: Parser ExprPA
  pHex = do
    p <- getNS
    _ <- char '0' >> char 'x'
    i <- hexadecimal
    t <- intTy
    pure $ LiteralPA p (IntLiteral i Hex t)

  pOct :: Parser ExprPA
  pOct = do
    p <- getNS
    _ <- char '0'
    i <- octal
    t <- intTy
    pure $ LiteralPA p (IntLiteral i Oct t)

  pInt :: Parser ExprPA
  pInt = do
    p <- getNS
    i <- integer
    t <- intTy
    pure $ LiteralPA p (IntLiteral i Dec t)

pString :: Parser ExprPA
pString = LiteralPA <$> getNS <*> (StrLiteral <$> stringLiteral)

pChar :: Parser ExprPA
pChar = LiteralPA <$> getNS <*> (CharLiteral <$> charLiteral)

pFuncCall :: Parser ExprPA
pFuncCall = do
  pos <- getNS
  name <- identifier
  args <- parens $ pExpr `sepBy` comma
  pure $ FunCallPA pos name args

pTerm :: Parser ExprPA
pTerm = pString
    <|> pInteger
    <|> pChar
    <|> pCast
    <|> try pAssign
    <|> try pFuncCall
    <|> try pArraySub
    <|> pIdentifier
    <|> pCIdentifier

pCast :: Parser ExprPA
pCast = do
  pos <- getNS
  typ <- parens pType
  expr <- pExpr
  pure $ CastPA pos typ expr

pIdentifier :: Parser ExprPA
pIdentifier = Identifier <$> getNS <*> identifier

pCIdentifier :: Parser ExprPA
pCIdentifier = IdentifierPA <$> getNS <*> cIdentifier

pArraySub :: Parser ExprPA
pArraySub = do
  pos <- getNS
  ident <- identifier <|> cIdentifier
  subsc <- bracks pExpr
  pure $ ArraySubPA pos ident subsc

pAssign :: Parser ExprPA
pAssign = do
  pos <- getNS
  left <- try pArraySub <|> pIdentifier <|> pCIdentifier
  _ <- equals
  right <- pExpr
  pure $ AssignPA pos left right

pExpr :: Parser ExprPA
pExpr = makeExprParser pTerm table >>= \case
  x@(MemberRef _ _ (Identifier _ _) _) -> pure x
  x@(MemberRef _ _ FunCall {} _)       -> pure x
  MemberRef {}                         -> customFailure $ ParserError "unexpected expression in member field access"
  x -> pure x
