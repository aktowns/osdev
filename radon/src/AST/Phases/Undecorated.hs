{-# LANGUAGE PatternSynonyms, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      : AST.Phases.Undecorated
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- The radon syntax tree - the bare undecorated nodes
-----------------------------------------------------------------------------
module AST.Phases.Undecorated where

import AST

data Undecorated

type ToplUD = TopLevel Undecorated
type ExprUD = Expression Undecorated
type StmtUD = Statement Undecorated

--------
-- Toplevel

type instance XEnum    Undecorated = ()
type instance XUnion   Undecorated = ()
type instance XStruct  Undecorated = ()
type instance XFunc    Undecorated = ()
type instance XDecl    Undecorated = ()
type instance XModule  Undecorated = ()
type instance XTypeDef Undecorated = ()
type instance XAlias   Undecorated = ()
type instance XImport  Undecorated = ()

pattern EnumUD :: Text -> [(Text, Maybe Integer)] -> ToplUD
pattern EnumUD i1 i2 <- Enum _ i1 i2
  where EnumUD i1 i2 = Enum () i1 i2

pattern UnionUD :: Text -> [(Text, [(Text, Type)])] -> ToplUD
pattern UnionUD i1 i2 <- Union _ i1 i2
  where UnionUD i1 i2 = Union () i1 i2

pattern StructUD :: Text -> [(Text, Type)] -> ToplUD
pattern StructUD i1 i2 <- Struct _ i1 i2
  where StructUD i1 i2 = Struct () i1 i2

pattern FuncUD :: Text -> Type -> ([(Text, Type)], Bool) -> [StmtUD] -> ToplUD
pattern FuncUD i1 i2 i3 i4 <- Func _ i1 i2 i3 i4
  where FuncUD i1 i2 i3 i4 = Func () i1 i2 i3 i4

pattern DeclUD :: Text -> Type -> Maybe (Expression Undecorated) -> ToplUD
pattern DeclUD i1 i2 i3 <- Decl _ i1 i2 i3
  where DeclUD i1 i2 i3 = Decl () i1 i2 i3

pattern ModuleUD :: Text -> [ToplUD] -> ToplUD
pattern ModuleUD i1 i2 <- Module _ i1 i2
  where ModuleUD i1 i2 = Module () i1 i2

pattern TypeDefUD :: Text -> Type -> ToplUD
pattern TypeDefUD i1 i2 <- TypeDef _ i1 i2
  where TypeDefUD i1 i2 = TypeDef () i1 i2

pattern AliasUD :: Maybe Language -> (Type, Text, ([Type], Bool)) -> Text -> ToplUD
pattern AliasUD i1 i2 i3 <- Alias _ i1 i2 i3
  where AliasUD i1 i2 i3 = Alias () i1 i2 i3

pattern ImportUD :: Maybe Language -> Text -> ToplUD
pattern ImportUD i1 i2 <- Import _ i1 i2
  where ImportUD i1 i2 = Import () i1 i2

--------
-- Expression

type instance XLiteral    Undecorated = ()
type instance XBinary     Undecorated = ()
type instance XUnary      Undecorated = ()
type instance XIdentifier Undecorated = ()
type instance XFunCall    Undecorated = ()
type instance XArraySub   Undecorated = ()
type instance XAssign     Undecorated = ()
type instance XCast       Undecorated = ()
type instance XMemberRef  Undecorated = ()

pattern LiteralUD :: Lit -> ExprUD
pattern LiteralUD i1 <- Literal _ i1
  where LiteralUD i1 = Literal () i1

pattern BinaryUD :: BinaryOp -> ExprUD -> ExprUD -> ExprUD
pattern BinaryUD i1 i2 i3 <- Binary _ i1 i2 i3
  where BinaryUD i1 i2 i3 = Binary () i1 i2 i3

pattern UnaryUD :: Fix -> UnaryOp -> ExprUD -> ExprUD
pattern UnaryUD i1 i2 i3 <- Unary _ i1 i2 i3
  where UnaryUD i1 i2 i3 = Unary () i1 i2 i3

pattern IdentifierUD :: Text -> ExprUD
pattern IdentifierUD i1 <- Identifier _ i1
  where IdentifierUD i1 = Identifier () i1

pattern FunCallUD :: Text -> [ExprUD] -> ExprUD
pattern FunCallUD i1 i2 <- FunCall _ i1 i2
  where FunCallUD i1 i2 = FunCall () i1 i2

pattern ArraySubUD :: Text -> ExprUD -> ExprUD
pattern ArraySubUD i1 i2 <- ArraySub _ i1 i2
  where ArraySubUD i1 i2 = ArraySub () i1 i2

pattern AssignUD :: ExprUD -> ExprUD -> ExprUD
pattern AssignUD i1 i2 <- Assign _ i1 i2
  where AssignUD i1 i2 = Assign () i1 i2

pattern CastUD :: Type -> ExprUD -> ExprUD
pattern CastUD i1 i2 <- Cast _ i1 i2
  where CastUD i1 i2 = Cast () i1 i2

pattern MemberRefUD :: MemberType -> ExprUD -> ExprUD -> ExprUD
pattern MemberRefUD i1 i2 i3 <- MemberRef _ i1 i2 i3
  where MemberRefUD i1 i2 i3 = MemberRef () i1 i2 i3

--------
-- Statement

type instance XDeclare Undecorated = ()
type instance XReturn  Undecorated = ()
type instance XWhile   Undecorated = ()
type instance XFor     Undecorated = ()
type instance XIf      Undecorated = ()
type instance XSExpr   Undecorated = ()

pattern DeclareUD :: Text -> Type -> Maybe ExprUD -> StmtUD
pattern DeclareUD i1 i2 i3 <- Declare _ i1 i2 i3
  where DeclareUD i1 i2 i3 = Declare () i1 i2 i3

pattern ReturnUD :: Maybe ExprUD -> StmtUD
pattern ReturnUD i1 <- Return _ i1
  where ReturnUD i1 = Return () i1

pattern WhileUD :: ExprUD -> [StmtUD] -> StmtUD
pattern WhileUD i1 i2 <- While _ i1 i2
  where WhileUD i1 i2 = While () i1 i2

pattern ForUD :: StmtUD -> ExprUD -> ExprUD -> [StmtUD] -> StmtUD
pattern ForUD i1 i2 i3 i4 <- For _ i1 i2 i3 i4
  where ForUD i1 i2 i3 i4 = For () i1 i2 i3 i4

pattern IfUD :: ExprUD -> [StmtUD] -> [(ExprUD, [StmtUD])] -> Maybe [StmtUD] -> StmtUD
pattern IfUD i1 i2 i3 i4 <- If _ i1 i2 i3 i4
  where IfUD i1 i2 i3 i4 = If () i1 i2 i3 i4

pattern SExprUD :: ExprUD -> StmtUD
pattern SExprUD i1 <- SExpr _ i1
  where SExprUD i1 = SExpr () i1
