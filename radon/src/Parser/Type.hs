{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.Type
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a parsing utilities for types
-----------------------------------------------------------------------------
module Parser.Type where

import Data.Functor ((<&>))

import Text.Megaparsec

import AST
import Parser.Common

pUnaryType :: Parser Type
pUnaryType = cIdentifier <&> \case
  "Void"   -> TyVoid
  "Char"   -> TyChar
  "String" -> TyPtr TyChar
  n        -> TyDef n

pWrapType :: Parser Type
pWrapType = do
  outer <- symbol "Ptr"
  inner <- angles pType
  return $ TyPtr inner

pType :: Parser Type
pType = pWrapType <|> pUnaryType

pVarQual :: Parser (Type -> Type)
pVarQual = (TyStatic <$ kStatic) <|> (TyInline <$ kInline) <|> (TyConst <$ kConst)

pFunQual :: Parser (Type -> Type)
pFunQual = (TyStatic <$ kStatic) <|> (TyInline <$ kInline)
