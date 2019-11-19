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
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Debug
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
pEnum = dbg "enum" $ L.indentBlock scn preamb
 where
  preamb = do
    pos <- getNA
    _ <- kEnum
    name <- cIdentifier
    _ <- equals
    return $ L.IndentSome Nothing (\x -> return $ Enum name x pos) pEnumBody

  pEnumBody :: Parser (Text, Maybe Integer)
  pEnumBody = (,) <$> cIdentifier <*> optional (equals *> integer)

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
pFunc = dbg "func" $ try pFuncSmall <|> pFuncFull
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

-- | Parses a val declaration
--
-- > val XYZ: String = "Hello"
-- or
--
-- > val XYZ: Int32
pDecl :: Parser TL
pDecl = dbg "decl" $ do
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
pModule = dbg "module" $ L.nonIndented scn (L.indentBlock scn preamb)
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
pTypeDef = dbg "type" $ do
  pos <- getNA
  _ <- kType
  ty <- cIdentifier
  _ <- equals 
  ty2 <- Left <$> pType
  return $ TypeDef ty ty2 pos

pTopLevelEntry :: Parser TL
pTopLevelEntry = try pEnum <|> try pFunc <|> try pDecl <|> try pModule <|> pTypeDef

pTopLevel :: Parser [TL]
pTopLevel = some $ L.nonIndented scn $ pTopLevelEntry <* space
