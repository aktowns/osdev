module AST where

import Language.C.Syntax.AST (Annotated(..))

import Data.Text (Text)

data Type = TyVoid 
          | TyInt
          | TyChar
          | TyString
          | TyPtr Type
          | TyDef Text
          | TyStatic Type
          | TyInline Type
          | TyConst Type
          deriving (Show)

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
              deriving Show

data UnaryOp = Negate
             | Positive
             | Increment
             deriving (Show)

data Fix = UnaryPrefix
         | UnaryPostfix
         deriving (Show)

data TopLevel a = Enum Text [(Text, Maybe Integer)] a
                | Func Text Type [(Text, Type)] [Stmt a] a
                | Decl Text Type (Maybe (Expr a)) a
                | Module Text [TopLevel a] a
                deriving (Show)

data Lit = IntLiteral Integer 
         | StrLiteral Text
         | CharLiteral Char
         deriving (Show)

data Expr a = Literal Lit a
            | Binary BinaryOp (Expr a) (Expr a) a
            | Unary Fix UnaryOp (Expr a) a
            | Identifier Text a
            | FunCall Text [Expr a] a
            | ArraySub Text (Expr a) a
            | Assign (Expr a) (Expr a) a
            deriving (Show)

data Stmt a = Declare Text Type (Maybe (Expr a)) a
            | Return (Maybe (Expr a)) a
            | While (Expr a) [Stmt a] a
            | For (Stmt a) (Expr a) (Expr a) [Stmt a] a
            | SExpr (Expr a) a
            deriving (Show)

instance Functor TopLevel where
  fmap f (Enum a1 a2 a3) = Enum a1 a2 (f a3)
  fmap f (Func a1 a2 a3 a4 a5) = Func a1 a2 a3 (fmap f <$> a4) (f a5)

instance Annotated TopLevel where
  annotation (Enum _ _ n)     = n
  annotation (Func _ _ _ _ n) = n

instance Functor Expr where
  fmap f (Literal a1 a2) = Literal a1 (f a2)
  fmap f (Binary a1 a2 a3 a4) = Binary a1 (fmap f a2) (fmap f a3) (f a4) 
  fmap f (FunCall a1 a2 a3) = FunCall a1 (fmap f <$> a2) (f a3) 
  fmap f (Assign a1 a2 a3)     = Assign (fmap f a1) (fmap f a2) (f a3)

instance Annotated Expr where
  annotation (Literal _ n)    = n
  annotation (Binary _ _ _ n) = n
  annotation (FunCall _ _ n)  = n
  annotation (Assign _ _ n)   = n

instance Functor Stmt where
  fmap f (Declare a1 a2 a3 a4) = Declare a1 a2 ((fmap . fmap) f a3) (f a4) 
  fmap f (Return a1 a2)        = Return ((fmap . fmap) f a1) (f a2)

instance Annotated Stmt where
  annotation (Declare _ _ _ n) = n
  annotation (Return _ n)      = n
