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

-- | Parses a struct
--
-- > struct StructType =
-- >   field1: Int32
-- >   field2: Int64
--
pStruct :: Parser TL
pStruct = L.indentBlock scn preamb
 where
  preamb = do
    pos <- getNA
    _ <- kStruct
    name <- cIdentifier
    _ <- equals
    return $ L.IndentSome Nothing (\x -> return $ Struct name x pos) pStructBody

  pStructBody :: Parser (Text, Type)
  pStructBody = (,) <$> identifier <*> (colon *> pType)

-- | Parses a union
--
-- > union UnionType =
-- >   ConStr1(x: Int32, y: Int32)
-- >   ConStr2(y: Int64)
-- >   ConStr3
--
pUnion :: Parser TL
pUnion = L.indentBlock scn preamb
 where
  preamb = do
    pos <- getNA
    _ <- kUnion
    name <- cIdentifier
    _ <- equals
    return $ L.IndentSome Nothing (\x -> return $ Union name x pos) pUnionBody

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
pFunc :: Parser TL
pFunc = try pFuncSmall <|> pFuncFull
 where
  pFuncSmall :: Parser TL
  pFuncSmall = do
    (pos, quals, name, args, typ) <- pFuncPreamb
    body <- pStmt
    return $ Func name (foldr ($) typ quals) args [body] pos

  pFuncFull :: Parser TL
  pFuncFull = L.indentBlock scn preamb
   where
    preamb = do
      (pos, quals, name, args, typ) <- pFuncPreamb
      return $ L.IndentSome Nothing (\x -> return $ Func name (foldr ($) typ quals) args x pos) pStmt

  pFuncPreamb :: Parser (NodeAnnotation, [Type -> Type], Text, ([(Text, Type)], Bool), Type)
  pFuncPreamb = do
      pos <- getNA
      quals <- many pFunQual
      name <- identifier
      args <- optional $ parens pArgs
      _ <- colon
      typ <- pType
      _ <- equals
      return (pos, quals, name, fromMaybe ([], False) args, typ)
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
pDecl :: Parser TL
pDecl = do
  pos <- getNA
  quals <- many pVarQual
  name <- kVal *> cIdentifier <* colon
  typ <- pType
  value <- optional $ equals *> pExpr
  return (Decl name (foldr ($) typ quals) value pos) <?> "val"

-- | parses top level module declarations
--
-- > module Hello =
-- >   val World: Int32 = 0
--
pModule :: Parser TL
pModule = L.nonIndented scn (L.indentBlock scn preamb)
 where
  preamb = do
    pos <- getNA
    name <- kModule *> cIdentifier <* equals
    return $ L.IndentSome Nothing (\x -> return $ Module name x pos) pTopLevelEntry

-- | parses a type definition
--
-- > type String = Ptr<Char>
-- or
--
-- > type UInt8 = [C]{ uint8_t }
--
pTypeDef :: Parser TL
pTypeDef = do
  pos <- getNA
  _ <- kType
  ty <- cIdentifier
  _ <- equals
  ty2 <- pType
  return $ TypeDef ty ty2 pos

pAlias :: Parser TL
pAlias = do
  pos <- getNA
  _ <- kAlias
  lang <- optional language
  ty <- pType
  name <- identifier
  args <- optional $ parens pArgs
  _ <- equals
  block <- identifier <|> cIdentifier
  return $ Alias lang (ty, name, fromMaybe ([], False) args) block pos
 where
  pArgs :: Parser ([Type], Bool)
  pArgs = try varArgs <|> justArgs
   where
    justArgs = (, False) <$> pType `sepBy` comma
    varArgs  = (,) <$> (pType `endBy` comma) <*> (True <$ varargs)

pImport :: Parser TL
pImport = do
  pos <- getNA
  _ <- kImport
  lang <- optional language
  file <- identifier <|> cIdentifier
  return $ Import lang file pos

pTopLevelEntry :: Parser TL
pTopLevelEntry = try pEnum
              <|> try pFunc
              <|> try pDecl
              <|> try pModule
              <|> try pStruct
              <|> try pUnion
              <|> pImport
              <|> pAlias
              <|> pTypeDef

pTopLevel :: Parser [TL]
pTopLevel = some $ L.nonIndented scn $ pTopLevelEntry <* space
