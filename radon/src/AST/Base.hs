{-# LANGUAGE StandaloneDeriving, DataKinds, GADTs, KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      : AST.Base
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- The radon syntax tree - Base tree
-----------------------------------------------------------------------------
module AST.Base where

data Type = TyVoid
          | TyPtr Type
          | TyDef Text
          | TyVar Text
          | TyFun Type Type
          | TyStatic Type
          | TyInline Type
          | TyConst Type
          | TyEmbedded (Embedded 'EType)
          deriving (Show, Eq, Ord)

data IntRep  = Dec | Hex | Oct deriving (Show, Eq, Ord)
data IntType = Unsigned | Long | LongLong deriving (Show, Eq, Ord)

data Lit = IntLiteral Integer IntRep [IntType]
         | StrLiteral Text
         | CharLiteral Char
         deriving (Show, Eq, Ord)

data Language = C deriving (Show, Eq, Ord)
data EmbeddedType = EExpr | EStmt | EType | ELit deriving (Show, Eq, Ord)

data Embedded :: EmbeddedType -> * where
  EmbeddedExpr :: Text -> Language -> Embedded 'EExpr
  EmbeddedStmt :: Text -> Language -> Embedded 'EStmt
  EmbeddedType :: Text -> Language -> Embedded 'EType
  EmbeddedLit  :: Text -> Language -> Embedded 'ELit

deriving instance Eq (Embedded a)
deriving instance Ord (Embedded a)
deriving instance Show (Embedded a)
