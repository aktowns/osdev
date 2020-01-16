{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds, RankNTypes #-}
{-# LANGUAGE GADTs, StandaloneDeriving, TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : AST.Expression
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- The radon syntax tree - Expressions
-----------------------------------------------------------------------------
module AST.Expression where

import GHC.Exts (Constraint)

import AST.Base

type family XLiteral x
type family XBinary x
type family XUnary x
type family XIdentifier x
type family XFunCall x
type family XArraySub x
type family XAssign x
type family XCast x
type family XMemberRef x

data BinaryOp = Add
              | Sub
              | Mul
              | Div
              | ShiftLeft
              | ShiftRight
              | LessThan
              | LessThanEqual
              | GreaterThan
              | GreaterThanEqual
              | Equals
              | NotEquals
              | BitwiseAnd
              | BitwiseXor
              | BitwiseOr
              | LogicalAnd
              | LogicalOr
              deriving (Show, Eq, Ord)

data UnaryOp = Negate
             | Positive
             | Increment
             | Decrement
             deriving (Show, Eq, Ord)

data Fix = UnaryPrefix
         | UnaryPostfix
         deriving (Show, Eq, Ord)

data MemberType = ModMem | PtrMem | FieldMem deriving (Show, Eq, Ord)

data Expression a = Literal !(XLiteral a) Lit
                  | Binary !(XBinary a) BinaryOp (Expression a) (Expression a)
                  | Unary !(XUnary a) Fix UnaryOp (Expression a)
                  | Identifier !(XIdentifier a) Text
                  | FunCall !(XFunCall a) (Expression a) [Expression a]
                  | ArraySub !(XArraySub a) Text (Expression a)
                  | Assign !(XAssign a) (Expression a) (Expression a)
                  | Cast !(XCast a) Type (Expression a)
                  | MemberRef !(XMemberRef a) MemberType (Expression a) (Expression a)

type ForallExpression (a :: * -> Constraint) l =
  ( a(XLiteral l), a(XBinary l), a(XUnary l), a(XIdentifier l), a(XFunCall l)
  , a(XArraySub l), a(XAssign l), a(XCast l), a(XMemberRef l)
  )

deriving instance ForallExpression Show a => Show (Expression a)
deriving instance ForallExpression Eq a => Eq (Expression a)
deriving instance ForallExpression Ord a => Ord (Expression a)
