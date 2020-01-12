{-# LANGUAGE PatternSynonyms, TypeFamilies, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      : AST.Phases.EnrichedC
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- pre-type check tree enrichment so we can resolve and check C types properly
-----------------------------------------------------------------------------
module AST.Phases.EnrichedC where

import AST
import AST.Phases.Undecorated

data EnrichedC

type ToplEC = TopLevel EnrichedC
type ExprEC = Expression EnrichedC
type StmtEC = Statement EnrichedC

data ResolvedImport = ResolvedImport { nodeSource :: NodeSource, defns :: [ToplEC] }

--------
-- Toplevel

type instance XEnum    EnrichedC = NodeSource
type instance XUnion   EnrichedC = NodeSource
type instance XStruct  EnrichedC = NodeSource
type instance XFunc    EnrichedC = NodeSource
type instance XDecl    EnrichedC = NodeSource
type instance XModule  EnrichedC = NodeSource
type instance XTypeDef EnrichedC = NodeSource
type instance XAlias   EnrichedC = NodeSource
type instance XImport  EnrichedC = ResolvedImport

pattern EnumEC :: NodeSource -> Text -> [(Text, Maybe Integer)] -> ToplEC
pattern EnumEC i1 i2 i3 <- Enum i1 i2 i3
  where EnumEC i1 i2 i3 = Enum i1 i2 i3

pattern UnionEC :: NodeSource -> Text -> [(Text, [(Text, Type)])] -> ToplEC
pattern UnionEC i1 i2 i3 <- Union i1 i2 i3
  where UnionEC i1 i2 i3 = Union i1 i2 i3

pattern StructEC :: NodeSource -> Text -> [(Text, Type)] -> ToplEC
pattern StructEC i1 i2 i3 <- Struct i1 i2 i3
  where StructEC i1 i2 i3 = Struct i1 i2 i3

pattern FuncEC :: NodeSource -> Text -> Type -> ([(Text, Type)], Bool) -> [StmtEC] -> ToplEC
pattern FuncEC i1 i2 i3 i4 i5 <- Func i1 i2 i3 i4 i5
  where FuncEC i1 i2 i3 i4 i5 = Func i1 i2 i3 i4 i5

pattern DeclEC :: NodeSource -> Text -> Type -> Maybe ExprEC -> ToplEC
pattern DeclEC i1 i2 i3 i4 <- Decl i1 i2 i3 i4
  where DeclEC i1 i2 i3 i4 = Decl i1 i2 i3 i4

pattern ModuleEC :: NodeSource -> Text -> [ToplEC] -> ToplEC
pattern ModuleEC i1 i2 i3 <- Module i1 i2 i3
  where ModuleEC i1 i2 i3 = Module i1 i2 i3

pattern TypeDefEC :: NodeSource -> Text -> Type -> ToplEC
pattern TypeDefEC i1 i2 i3 <- TypeDef i1 i2 i3
  where TypeDefEC i1 i2 i3 = TypeDef i1 i2 i3

pattern AliasEC :: NodeSource -> Maybe Language -> (Type, Text, ([Type], Bool)) -> Text -> ToplEC
pattern AliasEC i1 i2 i3 i4 <- Alias i1 i2 i3 i4
  where AliasEC i1 i2 i3 i4 = Alias i1 i2 i3 i4

pattern ImportEC :: ResolvedImport -> Maybe Language -> Text -> ToplEC
pattern ImportEC i1 i2 i3 <- Import i1 i2 i3
  where ImportEC i1 i2 i3 = Import i1 i2 i3

instance ToUndecorated TopLevel EnrichedC where
  toUndecorated (EnumEC _ a1 a2)       = EnumUD a1 a2
  toUndecorated (UnionEC _ a1 a2)      = UnionUD a1 a2
  toUndecorated (StructEC _ a1 a2)     = StructUD a1 a2
  toUndecorated (FuncEC _ a1 a2 a3 a4) = FuncUD a1 a2 a3 (toUndecorated <$> a4)
  toUndecorated (DeclEC _ a1 a2 a3)    = DeclUD a1 a2 (toUndecorated <$> a3)
  toUndecorated (ModuleEC _ a1 a2)     = ModuleUD a1 (toUndecorated <$> a2)
  toUndecorated (TypeDefEC _ a1 a2)    = TypeDefUD a1 a2
  toUndecorated (AliasEC _ a1 a2 a3)   = AliasUD a1 a2 a3
  toUndecorated (ImportEC _ a1 a2)     = ImportUD a1 a2

--------
-- Expression

type instance XLiteral    EnrichedC = NodeSource
type instance XBinary     EnrichedC = NodeSource
type instance XUnary      EnrichedC = NodeSource
type instance XIdentifier EnrichedC = NodeSource
type instance XFunCall    EnrichedC = NodeSource
type instance XArraySub   EnrichedC = NodeSource
type instance XAssign     EnrichedC = NodeSource
type instance XCast       EnrichedC = NodeSource
type instance XMemberRef  EnrichedC = NodeSource

pattern LiteralEC :: NodeSource -> Lit -> ExprEC
pattern LiteralEC i1 i2 <- Literal i1 i2
  where LiteralEC i1 i2 = Literal i1 i2

pattern BinaryEC :: NodeSource -> BinaryOp -> ExprEC -> ExprEC -> ExprEC
pattern BinaryEC i1 i2 i3 i4 <- Binary i1 i2 i3 i4
  where BinaryEC i1 i2 i3 i4 = Binary i1 i2 i3 i4

pattern UnaryEC :: NodeSource -> Fix -> UnaryOp -> ExprEC -> ExprEC
pattern UnaryEC i1 i2 i3 i4 <- Unary i1 i2 i3 i4
  where UnaryEC i1 i2 i3 i4 = Unary i1 i2 i3 i4

pattern IdentifierEC :: NodeSource -> Text -> ExprEC
pattern IdentifierEC i1 i2 <- Identifier i1 i2
  where IdentifierEC i1 i2 = Identifier i1 i2

pattern FunCallEC :: NodeSource -> Text -> [ExprEC] -> ExprEC
pattern FunCallEC i1 i2 i3 <- FunCall i1 i2 i3
  where FunCallEC i1 i2 i3 = FunCall i1 i2 i3

pattern ArraySubEC :: NodeSource -> Text -> ExprEC -> ExprEC
pattern ArraySubEC i1 i2 i3 <- ArraySub i1 i2 i3
  where ArraySubEC i1 i2 i3 = ArraySub i1 i2 i3

pattern AssignEC :: NodeSource -> ExprEC -> ExprEC -> ExprEC
pattern AssignEC i1 i2 i3 <- Assign i1 i2 i3
  where AssignEC i1 i2 i3 = Assign i1 i2 i3

pattern CastEC :: NodeSource -> Type -> ExprEC -> ExprEC
pattern CastEC i1 i2 i3 <- Cast i1 i2 i3
  where CastEC i1 i2 i3 = Cast i1 i2 i3

pattern MemberRefEC :: NodeSource -> MemberType -> ExprEC -> ExprEC -> ExprEC
pattern MemberRefEC i1 i2 i3 i4 <- MemberRef i1 i2 i3 i4
  where MemberRefEC i1 i2 i3 i4 = MemberRef i1 i2 i3 i4

instance ToUndecorated Expression EnrichedC where
  toUndecorated (LiteralEC _ a1)         = LiteralUD a1
  toUndecorated (BinaryEC _ a1 a2 a3)    = BinaryUD a1 (toUndecorated a2) (toUndecorated a3)
  toUndecorated (UnaryEC _ a1 a2 a3)     = UnaryUD a1 a2 (toUndecorated a3)
  toUndecorated (IdentifierEC _ a1)      = IdentifierUD a1
  toUndecorated (FunCallEC _ a1 a2)      = FunCallUD a1 (toUndecorated <$> a2)
  toUndecorated (ArraySubEC _ a1 a2)     = ArraySubUD a1 (toUndecorated a2)
  toUndecorated (AssignEC _ a1 a2)       = AssignUD (toUndecorated a1) (toUndecorated a2)
  toUndecorated (CastEC _ a1 a2)         = CastUD a1 (toUndecorated a2)
  toUndecorated (MemberRefEC _ a1 a2 a3) = MemberRefUD a1 (toUndecorated a2) (toUndecorated a3)

--------
-- Statement

type instance XDeclare EnrichedC = NodeSource
type instance XReturn  EnrichedC = NodeSource
type instance XWhile   EnrichedC = NodeSource
type instance XFor     EnrichedC = NodeSource
type instance XIf      EnrichedC = NodeSource
type instance XSExpr   EnrichedC = NodeSource

pattern DeclareEC :: NodeSource -> Text -> Type -> Maybe ExprEC -> StmtEC
pattern DeclareEC i1 i2 i3 i4 <- Declare i1 i2 i3 i4
  where DeclareEC i1 i2 i3 i4 = Declare i1 i2 i3 i4

pattern ReturnEC :: NodeSource -> Maybe ExprEC -> StmtEC
pattern ReturnEC i1 i2 <- Return i1 i2
  where ReturnEC i1 i2 = Return i1 i2

pattern WhileEC :: NodeSource -> ExprEC -> [StmtEC] -> StmtEC
pattern WhileEC i1 i2 i3 <- While i1 i2 i3
  where WhileEC i1 i2 i3 = While i1 i2 i3

pattern ForEC :: NodeSource -> StmtEC -> ExprEC -> ExprEC -> [StmtEC] -> StmtEC
pattern ForEC i1 i2 i3 i4 i5 <- For i1 i2 i3 i4 i5
  where ForEC i1 i2 i3 i4 i5 = For i1 i2 i3 i4 i5

pattern IfEC :: NodeSource -> ExprEC -> [StmtEC] -> [(ExprEC, [StmtEC])] -> Maybe [StmtEC] -> StmtEC
pattern IfEC i1 i2 i3 i4 i5 <- If i1 i2 i3 i4 i5
  where IfEC i1 i2 i3 i4 i5 = If i1 i2 i3 i4 i5

pattern SExprEC :: NodeSource -> ExprEC -> StmtEC
pattern SExprEC i1 i2 <- SExpr i1 i2
  where SExprEC i1 i2 = SExpr i1 i2

instance ToUndecorated Statement EnrichedC where
  toUndecorated (DeclareEC _ a1 a2 a3) = DeclareUD a1 a2 (toUndecorated <$> a3)
  toUndecorated (ReturnEC _ a1)        = ReturnUD (toUndecorated <$> a1)
  toUndecorated (WhileEC _ a1 a2)      = WhileUD (toUndecorated a1) (toUndecorated <$> a2)
  toUndecorated (ForEC _ a1 a2 a3 a4)  = ForUD (toUndecorated a1) (toUndecorated a2) (toUndecorated a3) (toUndecorated <$> a4)
  toUndecorated (IfEC _ a1 a2 a3 a4)   = IfUD (toUndecorated a1) (toUndecorated <$> a2) (bimap toUndecorated (fmap toUndecorated) <$> a3) ((fmap . fmap) toUndecorated a4)
  toUndecorated (SExprEC _ a1)         = SExprUD (toUndecorated a1)
