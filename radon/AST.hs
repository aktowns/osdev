module AST where

import Data.Text (Text)

data Type = TyVoid 
          | TyInt
          | TyChar
          | TyString
          | TyPtr Type
          | TyStatic Type
          | TyInline Type
          | TyConst Type
          deriving (Show)

data BinaryOp = Add deriving Show

data TopLevel = Enum Text [(Text, Maybe Integer)]
              | Func Text Type [(Text, Type)] [Node]
              deriving (Show)

data Lit = IntLiteral Integer deriving (Show)

data Expr = Literal Lit
          | Binary BinaryOp Expr Expr
          deriving (Show)

data Stmt = Assign Text Node
          | Declare Type Text (Maybe Expr)
          | Return (Maybe Expr)
          deriving (Show)

data Node = E Expr
          | S Stmt
          deriving (Show)

