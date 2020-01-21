{-# LANGUAGE PatternSynonyms, TypeFamilies, MultiParamTypeClasses, RecordWildCards #-}
module AST.Phases.Typed where

import AST
import AST.Phases.Parsed

data Typed

type ToplTC = TopLevel Typed
type ExprTC = Expression Typed
type StmtTC = Statement Typed

data TypedSource = TypedSource { nodeSource :: NodeSource
                               , typed      :: Type
                               }

instance Show TypedSource where
  show TypedSource{..} = show nodeSource <> " [" <> show typed <> "]"

--------
-- Toplevel

type instance XEnum    Typed = NodeSource
type instance XUnion   Typed = NodeSource
type instance XStruct  Typed = NodeSource
type instance XFunc    Typed = NodeSource
type instance XDecl    Typed = NodeSource
type instance XModule  Typed = NodeSource
type instance XTypeDef Typed = NodeSource
type instance XAlias   Typed = NodeSource
type instance XImport  Typed = NodeSource

pattern EnumTC :: NodeSource -> Text -> [(Text, Maybe Integer)] -> ToplTC
pattern EnumTC i1 i2 i3 <- Enum i1 i2 i3
  where EnumTC i1 i2 i3 = Enum i1 i2 i3

pattern UnionTC :: NodeSource -> Text -> [(Text, [(Text, Type)])] -> ToplTC
pattern UnionTC i1 i2 i3 <- Union i1 i2 i3
  where UnionTC i1 i2 i3 = Union i1 i2 i3

pattern StructTC :: NodeSource -> Text -> [(Text, Type)] -> ToplTC
pattern StructTC i1 i2 i3 <- Struct i1 i2 i3
  where StructTC i1 i2 i3 = Struct i1 i2 i3

pattern FuncTC :: NodeSource -> Text -> Type -> ([(Text, Type)], Bool) -> [StmtTC] -> ToplTC
pattern FuncTC i1 i2 i3 i4 i5 <- Func i1 i2 i3 i4 i5
  where FuncTC i1 i2 i3 i4 i5 = Func i1 i2 i3 i4 i5

pattern DeclTC :: NodeSource -> Text -> Type -> Maybe ExprTC -> ToplTC
pattern DeclTC i1 i2 i3 i4 <- Decl i1 i2 i3 i4
  where DeclTC i1 i2 i3 i4 = Decl i1 i2 i3 i4

pattern ModuleTC :: NodeSource -> Text -> [ToplTC] -> ToplTC
pattern ModuleTC i1 i2 i3 <- Module i1 i2 i3
  where ModuleTC i1 i2 i3 = Module i1 i2 i3

pattern TypeDefTC :: NodeSource -> Text -> Type -> ToplTC
pattern TypeDefTC i1 i2 i3 <- TypeDef i1 i2 i3
  where TypeDefTC i1 i2 i3 = TypeDef i1 i2 i3

pattern AliasTC :: NodeSource -> Maybe Language -> (Type, Text, ([Type], Bool)) -> Text -> ToplTC
pattern AliasTC i1 i2 i3 i4 <- Alias i1 i2 i3 i4
  where AliasTC i1 i2 i3 i4 = Alias i1 i2 i3 i4

pattern ImportTC :: NodeSource -> Maybe Language -> Text -> ToplTC
pattern ImportTC i1 i2 i3 <- Import i1 i2 i3
  where ImportTC i1 i2 i3 = Import i1 i2 i3

--------
-- Expression

type instance XLiteral    Typed = TypedSource
type instance XBinary     Typed = TypedSource
type instance XUnary      Typed = TypedSource
type instance XIdentifier Typed = TypedSource
type instance XFunCall    Typed = TypedSource
type instance XArraySub   Typed = TypedSource
type instance XAssign     Typed = TypedSource
type instance XCast       Typed = TypedSource
type instance XMemberRef  Typed = TypedSource

pattern LiteralTC :: TypedSource -> Lit -> ExprTC
pattern LiteralTC i1 i2 <- Literal i1 i2
  where LiteralTC i1 i2 = Literal i1 i2

pattern BinaryTC :: TypedSource -> BinaryOp -> ExprTC -> ExprTC -> ExprTC
pattern BinaryTC i1 i2 i3 i4 <- Binary i1 i2 i3 i4
  where BinaryTC i1 i2 i3 i4 = Binary i1 i2 i3 i4

pattern UnaryTC :: TypedSource -> Fix -> UnaryOp -> ExprTC -> ExprTC
pattern UnaryTC i1 i2 i3 i4 <- Unary i1 i2 i3 i4
  where UnaryTC i1 i2 i3 i4 = Unary i1 i2 i3 i4

pattern IdentifierTC :: TypedSource -> Text -> ExprTC
pattern IdentifierTC i1 i2 <- Identifier i1 i2
  where IdentifierTC i1 i2 = Identifier i1 i2

pattern FunCallTC :: TypedSource -> ExprTC -> [ExprTC] -> ExprTC
pattern FunCallTC i1 i2 i3 <- FunCall i1 i2 i3
  where FunCallTC i1 i2 i3 = FunCall i1 i2 i3

pattern ArraySubTC :: TypedSource -> Text -> ExprTC -> ExprTC
pattern ArraySubTC i1 i2 i3 <- ArraySub i1 i2 i3
  where ArraySubTC i1 i2 i3 = ArraySub i1 i2 i3

pattern AssignTC :: TypedSource -> ExprTC -> ExprTC -> ExprTC
pattern AssignTC i1 i2 i3 <- Assign i1 i2 i3
  where AssignTC i1 i2 i3 = Assign i1 i2 i3

pattern CastTC :: TypedSource -> Type -> ExprTC -> ExprTC
pattern CastTC i1 i2 i3 <- Cast i1 i2 i3
  where CastTC i1 i2 i3 = Cast i1 i2 i3

pattern MemberRefTC :: TypedSource -> MemberType -> ExprTC -> ExprTC -> ExprTC
pattern MemberRefTC i1 i2 i3 i4 <- MemberRef i1 i2 i3 i4
  where MemberRefTC i1 i2 i3 i4 = MemberRef i1 i2 i3 i4

--------
-- Statement

type instance XDeclare Typed = NodeSource
type instance XReturn  Typed = NodeSource
type instance XWhile   Typed = NodeSource
type instance XFor     Typed = NodeSource
type instance XIf      Typed = NodeSource
type instance XSExpr   Typed = NodeSource

pattern DeclareTC :: NodeSource -> Text -> Type -> Maybe ExprTC -> StmtTC
pattern DeclareTC i1 i2 i3 i4 <- Declare i1 i2 i3 i4
  where DeclareTC i1 i2 i3 i4 = Declare i1 i2 i3 i4

pattern ReturnTC :: NodeSource -> Maybe ExprTC -> StmtTC
pattern ReturnTC i1 i2 <- Return i1 i2
  where ReturnTC i1 i2 = Return i1 i2

pattern WhileTC :: NodeSource -> ExprTC -> [StmtTC] -> StmtTC
pattern WhileTC i1 i2 i3 <- While i1 i2 i3
  where WhileTC i1 i2 i3 = While i1 i2 i3

pattern ForTC :: NodeSource -> StmtTC -> ExprTC -> ExprTC -> [StmtTC] -> StmtTC
pattern ForTC i1 i2 i3 i4 i5 <- For i1 i2 i3 i4 i5
  where ForTC i1 i2 i3 i4 i5 = For i1 i2 i3 i4 i5

pattern IfTC :: NodeSource -> ExprTC -> [StmtTC] -> [(ExprTC, [StmtTC])] -> Maybe [StmtTC] -> StmtTC
pattern IfTC i1 i2 i3 i4 i5 <- If i1 i2 i3 i4 i5
  where IfTC i1 i2 i3 i4 i5 = If i1 i2 i3 i4 i5

pattern SExprTC :: NodeSource -> ExprTC -> StmtTC
pattern SExprTC i1 i2 <- SExpr i1 i2
  where SExprTC i1 i2 = SExpr i1 i2
