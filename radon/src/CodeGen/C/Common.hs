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

import Data.Text (Text)
import qualified Data.Text as T

import AST

un :: NodeInfo
un = undefNode

mkIdent' :: Text -> Name -> Ident
mkIdent' x = mkIdent nopos (T.unpack x)

toNI :: NodeAnnotation -> NodeInfo
toNI NodeAnnotation{..} = mkNodeInfoOnlyPos (position 0 filename line column Nothing)
