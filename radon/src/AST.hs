-----------------------------------------------------------------------------
-- |
-- Module      :  AST
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- The radon syntax tree
-----------------------------------------------------------------------------
module AST where

import Language.C.Syntax.AST (Annotated(..))

import Data.Text (Text)

data NodeAnnotation = NodeAnnotation { filename :: String
                                     , line     :: Int
                                     , column   :: Int
                                     } deriving (Show)

data Type = TyVoid
          | TyChar
          | TyString
          | TyPtr Type
          | TyDef Text
          | TyStatic Type
          | TyInline Type
          | TyConst Type
          deriving (Show, Eq, Ord)

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

data TopLevel a = Enum Text [(Text, Maybe Integer)] a
                | Func Text Type [(Text, Type)] [Statement a] a
                | Decl Text Type (Maybe (Expression a)) a
                | Module Text [TopLevel a] a
                deriving (Show, Eq, Ord)

type TL = TopLevel NodeAnnotation

data IntRep  = Dec | Hex | Oct deriving (Show, Eq, Ord)
data IntType = Unsigned | Long | LongLong deriving (Show, Eq, Ord)

data Lit = IntLiteral Integer IntRep [IntType]
         | StrLiteral Text
         | CharLiteral Char
         deriving (Show, Eq, Ord)

data Expression a = Literal Lit a
                  | Binary BinaryOp (Expression a) (Expression a) a
                  | Unary Fix UnaryOp (Expression a) a
                  | Identifier Text a
                  | FunCall Text [Expression a] a
                  | ArraySub Text (Expression a) a
                  | Assign (Expression a) (Expression a) a
                  | Cast Type (Expression a) a
                  deriving (Show, Eq, Ord)

type Expr = Expression NodeAnnotation

data Statement a = Declare Text Type (Maybe (Expression a)) a
                 | Return (Maybe (Expression a)) a
                 | While (Expression a) [Statement a] a
                 | For (Statement a) (Expression a) (Expression a) [Statement a] a
                 | SExpr (Expression a) a
                 deriving (Show, Eq, Ord)

type Stmt = Statement NodeAnnotation

instance Functor TopLevel where
  fmap f (Enum a1 a2 a3)       = Enum a1 a2 (f a3)
  fmap f (Func a1 a2 a3 a4 a5) = Func a1 a2 a3 (fmap f <$> a4) (f a5)
  fmap f (Decl a1 a2 a3 a4)    = Decl a1 a2 ((fmap . fmap) f a3) (f a4)
  fmap f (Module a1 a2 a3)     = Module a1 ((fmap . fmap) f a2) (f a3)

instance Annotated TopLevel where
  annotation (Enum _ _ n)     = n
  annotation (Func _ _ _ _ n) = n
  annotation (Decl _ _ _ n)   = n
  annotation (Module _ _ n)   = n

  amap f (Enum a1 a2 a3)       = Enum a1 a2 $ f a3
  amap f (Func a1 a2 a3 a4 a5) = Func a1 a2 a3 a4 $ f a5
  amap f (Decl a1 a2 a3 a4)    = Decl a1 a2 a3 $ f a4
  amap f (Module a1 a2 a3)     = Module a1 a2 $ f a3

instance Functor Expression where
  fmap f (Literal a1 a2)      = Literal a1 (f a2)
  fmap f (Binary a1 a2 a3 a4) = Binary a1 (fmap f a2) (fmap f a3) (f a4)
  fmap f (FunCall a1 a2 a3)   = FunCall a1 (fmap f <$> a2) (f a3)
  fmap f (Assign a1 a2 a3)    = Assign (fmap f a1) (fmap f a2) (f a3)
  fmap f (ArraySub a1 a2 a3)  = ArraySub a1 (fmap f a2) (f a3)
  fmap f (Unary a1 a2 a3 a4)  = Unary a1 a2 (fmap f a3) (f a4)
  fmap f (Identifier a1 a2)   = Identifier a1 (f a2)
  fmap f (Cast a1 a2 a3)      = Cast a1 (fmap f a2) (f a3)

instance Annotated Expression where
  annotation (Literal _ n)    = n
  annotation (Binary _ _ _ n) = n
  annotation (FunCall _ _ n)  = n
  annotation (Assign _ _ n)   = n
  annotation (ArraySub _ _ n) = n
  annotation (Unary _ _ _ n)  = n
  annotation (Identifier _ n) = n
  annotation (Cast _ _ n)     = n

  amap f (Literal a1 a2)      = Literal a1 $ f a2
  amap f (Binary a1 a2 a3 a4) = Binary a1 a2 a3 $ f a4
  amap f (FunCall a1 a2 a3)   = FunCall a1 a2 $ f a3
  amap f (Assign a1 a2 a3)    = Assign a1 a2 $ f a3
  amap f (ArraySub a1 a2 a3)  = ArraySub a1 a2 $ f a3
  amap f (Unary a1 a2 a3 a4)  = Unary a1 a2 a3 $ f a4
  amap f (Identifier a1 a2)   = Identifier a1 $ f a2
  amap f (Cast a1 a2 a3)      = Cast a1 a2 $ f a3

instance Functor Statement where
  fmap f (Declare a1 a2 a3 a4) = Declare a1 a2 ((fmap . fmap) f a3) (f a4)
  fmap f (Return a1 a2)        = Return ((fmap . fmap) f a1) (f a2)
  fmap f (While a1 a2 a3)      = While (fmap f a1) ((fmap . fmap) f a2) (f a3)
  fmap f (For a1 a2 a3 a4 a5)  = For (fmap f a1) (fmap f a2) (fmap f a3) ((fmap . fmap) f a4) (f a5)
  fmap f (SExpr a1 a2)         = SExpr (fmap f a1) (f a2)
  fmap f (While a1 a2 a3)      = While (fmap f a1) ((fmap . fmap) f a2) (f a3)

instance Annotated Statement where
  annotation (Declare _ _ _ n) = n
  annotation (Return _ n)      = n
  annotation (While _ _ n)     = n
  annotation (For _ _ _ _ n)   = n
  annotation (SExpr _ n)       = n

  amap f (Declare a1 a2 a3 a4) = Declare a1 a2 a3 $ f a4
  amap f (Return a1 a2)        = Return a1 $ f a2
  amap f (While a1 a2 a3)      = While a1 a2 $ f a3
  amap f (For a1 a2 a3 a4 a5)  = For a1 a2 a3 a4 $ f a5
  amap f (SExpr a1 a2)         = SExpr a1 $ f a2
