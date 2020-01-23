{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CodeGen.C.TopLevel
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides common C code generation for radon utils
-----------------------------------------------------------------------------
module CodeGen.C.Common where

import Language.C.Data.Node
import Language.C.Data.Name
import Language.C.Data.Ident
import Language.C.Data.Position

import AST

un :: NodeInfo
un = undefNode

mkIdent' :: Text -> Name -> Ident
mkIdent' x = mkIdent nopos (toS x)

class ToNI a where
  toNI :: a -> NodeInfo

instance ToNI NodeSource where
  toNI NodeSource{..} = mkNodeInfoOnlyPos (position 0 (toS filename) line column Nothing)

instance ToNI TypedSource where
  toNI TypedSource{nodeSource} = toNI nodeSource
