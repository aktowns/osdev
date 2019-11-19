{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CodeGen.C.Type
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides code generation for C types
-----------------------------------------------------------------------------
module CodeGen.C.Type where

import Data.Bifunctor (first, second)

import Language.C.Data.Name
import Language.C.Syntax.AST

import AST
import CodeGen.C.Common

evalType :: Type -> ([CDeclSpec], [CDerivedDeclr])
evalType TyVoid        = ([CTypeSpec (CVoidType un)], [])
evalType TyChar        = ([CTypeSpec (CCharType un)], [])
evalType (TyDef ty)    = ([CTypeSpec (CTypeDef (mkIdent' ty (Name 0)) un)], [])
evalType (TyPtr ty)    = second (\x -> CPtrDeclr [] un : x) $ evalType ty
evalType (TyInline ty) = first (\x -> CFunSpec (CInlineQual un) : x) $ evalType ty
evalType (TyStatic ty) = first (\x -> CStorageSpec (CStatic un) : x) $ evalType ty
evalType (TyConst ty)  = first (\x -> CTypeQual (CConstQual un) : x) $ evalType ty
evalType (TyEmbedded (EmbeddedType n C)) =
  ([CTypeSpec (CTypeDef (mkIdent' n (Name 0)) un)], [])
evalType x             = error $ "unhandled " ++ show x
