{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds, RankNTypes #-}
{-# LANGUAGE GADTs, StandaloneDeriving, TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : AST.TopLevel
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- The radon syntax tree - toplevel nodes
-----------------------------------------------------------------------------
module AST.TopLevel where

import GHC.Exts (Constraint)

import AST.Base
import AST.Statement
import AST.Expression

type family XEnum x
type family XUnion x
type family XStruct x
type family XFunc x
type family XDecl x
type family XModule x
type family XTypeDef x
type family XAlias x
type family XImport x

data TopLevel a = Enum    !(XEnum a) Text [(Text, Maybe Integer)]
                | Union   !(XUnion a) Text [(Text, [(Text, Type)])]
                | Struct  !(XStruct a) Text [(Text, Type)]
                | Func    !(XFunc a) Text Type ([(Text, Type)], Bool) [Statement a]
                | Decl    !(XDecl a) Text Type (Maybe (Expression a))
                | Module  !(XModule a) Text [TopLevel a]
                | TypeDef !(XTypeDef a) Text Type
                | Alias   !(XAlias a) (Maybe Language) (Type, Text, ([Type], Bool)) Text
                | Import  !(XImport a) (Maybe Language) Text

type ForallTopLevel (a :: * -> Constraint) l = ( a(XEnum l), a(XUnion l), a(XStruct l), a(XFunc l), a(XDecl l), a(XModule l)
                                               , a(XTypeDef l), a(XAlias l), a(XImport l), ForallStatement a l
                                               )

deriving instance ForallTopLevel Show a => Show (TopLevel a)
deriving instance ForallTopLevel Eq a => Eq (TopLevel a)
deriving instance ForallTopLevel Ord a => Ord (TopLevel a)


---------
-- undecorated tree

