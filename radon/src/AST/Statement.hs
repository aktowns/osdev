{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds, RankNTypes #-}
{-# LANGUAGE GADTs, StandaloneDeriving, TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : AST.Statement
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- The radon syntax tree - statements
-----------------------------------------------------------------------------
module AST.Statement where

import GHC.Exts (Constraint)

import AST.Base
import AST.Expression

type family XDeclare x
type family XReturn x
type family XWhile x
type family XFor x
type family XIf x
type family XSExpr x

data Statement a = Declare !(XDeclare a) Text Type (Maybe (Expression a))
                 | Return !(XReturn a) (Maybe (Expression a))
                 | While !(XWhile a) (Expression a) [Statement a]
                 | For !(XFor a) (Statement a) (Expression a) (Expression a) [Statement a]
                 | If !(XIf a) (Expression a) [Statement a] [(Expression a, [Statement a])] (Maybe [Statement a])
                 | SExpr !(XSExpr a) (Expression a)

type ForallStatement (a :: * -> Constraint) l =
  ( a(XDeclare l), a(XReturn l), a(XWhile l), a(XFor l), a(XIf l), a(XSExpr l), ForallExpression a l)

deriving instance ForallStatement Show a => Show (Statement a)
deriving instance ForallStatement Eq a => Eq (Statement a)
deriving instance ForallStatement Ord a => Ord (Statement a)
