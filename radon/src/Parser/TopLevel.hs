{-# LANGUAGE TupleSections #-}
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

import Control.Applicative hiding (some, many)

import Text.Megaparsec
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import AST.Phases.Parsed

import Parser.Common
import Parser.Expression
import Parser.Type
import Parser.Statement

-- | Parses an enum
--
-- > enum EnumType =
-- >   Black = 1
-- >   Blue  = 2
-- or
--
-- > enum EnumType =
-- >   Black
-- >   Blue
pEnum :: Parser ToplPA
pEnum = L.indentBlock scn preamb
 where
  preamb = do
    pos <- getNS
    _ <- kEnum
    name <- cIdentifier
    _ <- equals
    pure $ L.IndentSome Nothing (pure . EnumPA pos name) pEnumBody

  pEnumBody :: Parser (Text, Maybe Integer)
  pEnumBody = (,) <$> cIdentifier <*> optional (equals *> integer)

-- | Parses a struct
--
-- > struct StructType =
-- >   field1: Int32
-- >   field2: Int64
--
pStruct :: Parser ToplPA
pStruct = L.indentBlock scn preamb
 where
  preamb = do
    pos <- getNS
    _ <- kStruct
    name <- cIdentifier
    _ <- equals
    pure $ L.IndentSome Nothing (pure . StructPA pos name) pStructBody

  pStructBody :: Parser (Text, Type)
  pStructBody = (,) <$> identifier <*> (colon *> pType)

-- | Parses a union
--
-- > union UnionType =
-- >   ConStr1(x: Int32, y: Int32)
-- >   ConStr2(y: Int64)
-- >   ConStr3
--
pUnion :: Parser ToplPA
pUnion = L.indentBlock scn preamb
 where
  preamb = do
    pos <- getNS
    _ <- kUnion
    name <- cIdentifier
    _ <- equals
    pure $ L.IndentSome Nothing (pure . UnionPA pos name) pUnionBody

  pUnionBody :: Parser (Text, [(Text, Type)])
  pUnionBody = (,) <$> cIdentifier <*> (fromMaybe [] <$> optional (parens pArgs))

  pArg :: Parser (Text, Type)
  pArg = (,) <$> identifier <* colon <*> pType

  pArgs :: Parser [(Text, Type)]
  pArgs = pArg `sepBy` comma

-- | Parses a function
--
-- > functionName: ReturnType =
-- >   body
-- or
--
-- > functionName(arg: Type): ReturnType = statement
-- or
--
-- > functionName(arg: Type): ReturnType =
-- >   body
pFunc :: Parser ToplPA
pFunc = try pFuncSmall <|> pFuncFull
 where
  pFuncSmall :: Parser ToplPA
  pFuncSmall = do
    (pos, quals, name, args, typ) <- pFuncPreamb
    body <- pStmt
    pure $ FuncPA pos name (foldr ($) typ quals) args [body]

  pFuncFull :: Parser ToplPA
  pFuncFull = L.indentBlock scn preamb
   where
    preamb = do
      (pos, quals, name, args, typ) <- pFuncPreamb
      pure $ L.IndentSome Nothing (pure . FuncPA pos name (foldr ($) typ quals) args) pStmt

  pFuncPreamb :: Parser (NodeSource, [Type -> Type], Text, ([(Text, Type)], Bool), Type)
  pFuncPreamb = do
      pos <- getNS
      quals <- many pFunQual
      name <- identifier
      args <- optional $ parens pArgs
      _ <- colon
      typ <- pType
      _ <- equals
      pure (pos, quals, name, fromMaybe ([], False) args, typ)
   where
    pArg :: Parser (Text, Type)
    pArg = (,) <$> identifier <* colon <*> pType

    pArgs :: Parser ([(Text, Type)], Bool)
    pArgs = try varArgs <|> justArgs
     where
      justArgs = (, False) <$> pArg `sepBy` comma
      varArgs  = (,) <$> (pArg `endBy` comma) <*> (True <$ varargs)

-- | Parses a val declaration
--
-- > val XYZ: String = "Hello"
-- or
--
-- > val XYZ: Int32
pDecl :: Parser ToplPA
pDecl = do
  pos <- getNS
  quals <- many pVarQual
  name <- kVal *> cIdentifier <* colon
  typ <- pType
  value <- optional $ equals *> pExpr
  pure (DeclPA pos name (foldr ($) typ quals) value) <?> "val"

-- | parses top level module declarations
--
-- > module Hello =
-- >   val World: Int32 = 0
--
pModule :: Parser ToplPA
pModule = L.nonIndented scn (L.indentBlock scn preamb)
 where
  preamb = do
    pos <- getNS
    name <- kModule *> cIdentifier <* equals
    pure $ L.IndentSome Nothing (pure . ModulePA pos name) pTopLevelEntry

-- | parses a type definition
--
-- > type String = Ptr<Char>
-- or
--
-- > type UInt8 = [C]{ uint8_t }
--
pTypeDef :: Parser ToplPA
pTypeDef = do
  pos <- getNS
  _ <- kType
  ty <- cIdentifier
  _ <- equals
  ty2 <- pType
  pure $ TypeDef pos ty ty2

pAlias :: Parser ToplPA
pAlias = do
  pos <- getNS
  _ <- kAlias
  lang <- optional language
  ty <- pType
  name <- identifier
  args <- optional $ parens pArgs
  _ <- equals
  block <- identifier <|> cIdentifier
  pure $ AliasPA pos lang (ty, name, fromMaybe ([], False) args) block
 where
  pArgs :: Parser ([Type], Bool)
  pArgs = try varArgs <|> justArgs
   where
    justArgs = (, False) <$> pType `sepBy` comma
    varArgs  = (,) <$> (pType `endBy` comma) <*> (True <$ varargs)

pImport :: Parser ToplPA
pImport = do
  pos <- getNS
  _ <- kImport
  lang <- optional language
  file <- identifier <|> cIdentifier
  pure $ ImportPA pos lang file

pTopLevelEntry :: Parser ToplPA
pTopLevelEntry = try pEnum
              <|> try pFunc
              <|> try pDecl
              <|> try pModule
              <|> try pStruct
              <|> try pUnion
              <|> pImport
              <|> pAlias
              <|> pTypeDef

pTopLevel :: Parser [ToplPA]
pTopLevel = some $ L.nonIndented scn $ pTopLevelEntry <* space
