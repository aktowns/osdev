{-# LANGUAGE PatternSynonyms, TypeFamilies, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      : AST.Phases.Parsed
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- The radon syntax tree - parsed nodes
-----------------------------------------------------------------------------
module AST.Phases.Parsed where

import AST
import AST.Phases.Undecorated

data Parsed

type ToplPA = TopLevel Parsed
type ExprPA = Expression Parsed
type StmtPA = Statement Parsed

--------
-- Toplevel

type instance XEnum    Parsed = NodeSource
type instance XUnion   Parsed = NodeSource
type instance XStruct  Parsed = NodeSource
type instance XFunc    Parsed = NodeSource
type instance XDecl    Parsed = NodeSource
type instance XModule  Parsed = NodeSource
type instance XTypeDef Parsed = NodeSource
type instance XAlias   Parsed = NodeSource
type instance XImport  Parsed = NodeSource

pattern EnumPA :: NodeSource -> Text -> [(Text, Maybe Integer)] -> ToplPA
pattern EnumPA i1 i2 i3 <- Enum i1 i2 i3
  where EnumPA i1 i2 i3 = Enum i1 i2 i3

pattern UnionPA :: NodeSource -> Text -> [(Text, [(Text, Type)])] -> ToplPA
pattern UnionPA i1 i2 i3 <- Union i1 i2 i3
  where UnionPA i1 i2 i3 = Union i1 i2 i3

pattern StructPA :: NodeSource -> Text -> [(Text, Type)] -> ToplPA
pattern StructPA i1 i2 i3 <- Struct i1 i2 i3
  where StructPA i1 i2 i3 = Struct i1 i2 i3

pattern FuncPA :: NodeSource -> Text -> Type -> ([(Text, Type)], Bool) -> [StmtPA] -> ToplPA
pattern FuncPA i1 i2 i3 i4 i5 <- Func i1 i2 i3 i4 i5
  where FuncPA i1 i2 i3 i4 i5 = Func i1 i2 i3 i4 i5

pattern DeclPA :: NodeSource -> Text -> Type -> Maybe ExprPA -> ToplPA
pattern DeclPA i1 i2 i3 i4 <- Decl i1 i2 i3 i4
  where DeclPA i1 i2 i3 i4 = Decl i1 i2 i3 i4

pattern ModulePA :: NodeSource -> Text -> [ToplPA] -> ToplPA
pattern ModulePA i1 i2 i3 <- Module i1 i2 i3
  where ModulePA i1 i2 i3 = Module i1 i2 i3

pattern TypeDefPA :: NodeSource -> Text -> Type -> ToplPA
pattern TypeDefPA i1 i2 i3 <- TypeDef i1 i2 i3
  where TypeDefPA i1 i2 i3 = TypeDef i1 i2 i3

pattern AliasPA :: NodeSource -> Maybe Language -> (Type, Text, ([Type], Bool)) -> Text -> ToplPA
pattern AliasPA i1 i2 i3 i4 <- Alias i1 i2 i3 i4
  where AliasPA i1 i2 i3 i4 = Alias i1 i2 i3 i4

pattern ImportPA :: NodeSource -> Maybe Language -> Text -> ToplPA
pattern ImportPA i1 i2 i3 <- Import i1 i2 i3
  where ImportPA i1 i2 i3 = Import i1 i2 i3

instance ToUndecorated TopLevel Parsed where
  toUndecorated (EnumPA _ a1 a2)       = EnumUD a1 a2
  toUndecorated (UnionPA _ a1 a2)      = UnionUD a1 a2
  toUndecorated (StructPA _ a1 a2)     = StructUD a1 a2
  toUndecorated (FuncPA _ a1 a2 a3 a4) = FuncUD a1 a2 a3 (toUndecorated <$> a4)
  toUndecorated (DeclPA _ a1 a2 a3)    = DeclUD a1 a2 (toUndecorated <$> a3)
  toUndecorated (ModulePA _ a1 a2)     = ModuleUD a1 (toUndecorated <$> a2)
  toUndecorated (TypeDefPA _ a1 a2)    = TypeDefUD a1 a2
  toUndecorated (AliasPA _ a1 a2 a3)   = AliasUD a1 a2 a3
  toUndecorated (ImportPA _ a1 a2)     = ImportUD a1 a2

--------
-- Expression

type instance XLiteral    Parsed = NodeSource
type instance XBinary     Parsed = NodeSource
type instance XUnary      Parsed = NodeSource
type instance XIdentifier Parsed = NodeSource
type instance XFunCall    Parsed = NodeSource
type instance XArraySub   Parsed = NodeSource
type instance XAssign     Parsed = NodeSource
type instance XCast       Parsed = NodeSource
type instance XMemberRef  Parsed = NodeSource

pattern LiteralPA :: NodeSource -> Lit -> ExprPA
pattern LiteralPA i1 i2 <- Literal i1 i2
  where LiteralPA i1 i2 = Literal i1 i2

pattern BinaryPA :: NodeSource -> BinaryOp -> ExprPA -> ExprPA -> ExprPA
pattern BinaryPA i1 i2 i3 i4 <- Binary i1 i2 i3 i4
  where BinaryPA i1 i2 i3 i4 = Binary i1 i2 i3 i4

pattern UnaryPA :: NodeSource -> Fix -> UnaryOp -> ExprPA -> ExprPA
pattern UnaryPA i1 i2 i3 i4 <- Unary i1 i2 i3 i4
  where UnaryPA i1 i2 i3 i4 = Unary i1 i2 i3 i4

pattern IdentifierPA :: NodeSource -> Text -> ExprPA
pattern IdentifierPA i1 i2 <- Identifier i1 i2
  where IdentifierPA i1 i2 = Identifier i1 i2

pattern FunCallPA :: NodeSource -> Text -> [ExprPA] -> ExprPA
pattern FunCallPA i1 i2 i3 <- FunCall i1 i2 i3
  where FunCallPA i1 i2 i3 = FunCall i1 i2 i3

pattern ArraySubPA :: NodeSource -> Text -> ExprPA -> ExprPA
pattern ArraySubPA i1 i2 i3 <- ArraySub i1 i2 i3
  where ArraySubPA i1 i2 i3 = ArraySub i1 i2 i3

pattern AssignPA :: NodeSource -> ExprPA -> ExprPA -> ExprPA
pattern AssignPA i1 i2 i3 <- Assign i1 i2 i3
  where AssignPA i1 i2 i3 = Assign i1 i2 i3

pattern CastPA :: NodeSource -> Type -> ExprPA -> ExprPA
pattern CastPA i1 i2 i3 <- Cast i1 i2 i3
  where CastPA i1 i2 i3 = Cast i1 i2 i3

pattern MemberRefPA :: NodeSource -> MemberType -> ExprPA -> ExprPA -> ExprPA
pattern MemberRefPA i1 i2 i3 i4 <- MemberRef i1 i2 i3 i4
  where MemberRefPA i1 i2 i3 i4 = MemberRef i1 i2 i3 i4

instance ToUndecorated Expression Parsed where
  toUndecorated (LiteralPA _ a1)         = LiteralUD a1
  toUndecorated (BinaryPA _ a1 a2 a3)    = BinaryUD a1 (toUndecorated a2) (toUndecorated a3)
  toUndecorated (UnaryPA _ a1 a2 a3)     = UnaryUD a1 a2 (toUndecorated a3)
  toUndecorated (IdentifierPA _ a1)      = IdentifierUD a1
  toUndecorated (FunCallPA _ a1 a2)      = FunCallUD a1 (toUndecorated <$> a2)
  toUndecorated (ArraySubPA _ a1 a2)     = ArraySubUD a1 (toUndecorated a2)
  toUndecorated (AssignPA _ a1 a2)       = AssignUD (toUndecorated a1) (toUndecorated a2)
  toUndecorated (CastPA _ a1 a2)         = CastUD a1 (toUndecorated a2)
  toUndecorated (MemberRefPA _ a1 a2 a3) = MemberRefUD a1 (toUndecorated a2) (toUndecorated a3)

--------
-- Statement

type instance XDeclare Parsed = NodeSource
type instance XReturn  Parsed = NodeSource
type instance XWhile   Parsed = NodeSource
type instance XFor     Parsed = NodeSource
type instance XIf      Parsed = NodeSource
type instance XSExpr   Parsed = NodeSource

pattern DeclarePA :: NodeSource -> Text -> Type -> Maybe ExprPA -> StmtPA
pattern DeclarePA i1 i2 i3 i4 <- Declare i1 i2 i3 i4
  where DeclarePA i1 i2 i3 i4 = Declare i1 i2 i3 i4

pattern ReturnPA :: NodeSource -> Maybe ExprPA -> StmtPA
pattern ReturnPA i1 i2 <- Return i1 i2
  where ReturnPA i1 i2 = Return i1 i2

pattern WhilePA :: NodeSource -> ExprPA -> [StmtPA] -> StmtPA
pattern WhilePA i1 i2 i3 <- While i1 i2 i3
  where WhilePA i1 i2 i3 = While i1 i2 i3

pattern ForPA :: NodeSource -> StmtPA -> ExprPA -> ExprPA -> [StmtPA] -> StmtPA
pattern ForPA i1 i2 i3 i4 i5 <- For i1 i2 i3 i4 i5
  where ForPA i1 i2 i3 i4 i5 = For i1 i2 i3 i4 i5

pattern IfPA :: NodeSource -> ExprPA -> [StmtPA] -> [(ExprPA, [StmtPA])] -> Maybe [StmtPA] -> StmtPA
pattern IfPA i1 i2 i3 i4 i5 <- If i1 i2 i3 i4 i5
  where IfPA i1 i2 i3 i4 i5 = If i1 i2 i3 i4 i5

pattern SExprPA :: NodeSource -> ExprPA -> StmtPA
pattern SExprPA i1 i2 <- SExpr i1 i2
  where SExprPA i1 i2 = SExpr i1 i2

instance ToUndecorated Statement Parsed where
  toUndecorated (DeclarePA _ a1 a2 a3) = DeclareUD a1 a2 (toUndecorated <$> a3)
  toUndecorated (ReturnPA _ a1)        = ReturnUD (toUndecorated <$> a1)
  toUndecorated (WhilePA _ a1 a2)      = WhileUD (toUndecorated a1) (toUndecorated <$> a2)
  toUndecorated (ForPA _ a1 a2 a3 a4)  = ForUD (toUndecorated a1) (toUndecorated a2) (toUndecorated a3) (toUndecorated <$> a4)
  toUndecorated (IfPA _ a1 a2 a3 a4)   = IfUD (toUndecorated a1) (toUndecorated <$> a2) (bimap toUndecorated (fmap toUndecorated) <$> a3) ((fmap . fmap) toUndecorated a4)
  toUndecorated (SExprPA _ a1)         = SExprUD (toUndecorated a1)
