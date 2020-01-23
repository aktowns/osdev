-----------------------------------------------------------------------------
-- |
-- Module      :  CodeGen.C.Expression
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides C code generation for radon expressions
-----------------------------------------------------------------------------
module CodeGen.C.Expression where

import Language.C.Data.Name
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants

import AST
import AST.Phases.Typed
import CodeGen.C.Common
import CodeGen.C.Type

assign :: ExprTC -> ExprTC -> NodeInfo -> CExpr
assign n v = CAssign CAssignOp (evalExpr n) (evalExpr v)

evalBinaryOp :: BinaryOp -> CBinaryOp
evalBinaryOp Add        = CAddOp
evalBinaryOp Sub        = CSubOp
evalBinaryOp Mul        = CMulOp
evalBinaryOp Div        = CDivOp
evalBinaryOp LessThan   = CLeOp
evalBinaryOp ShiftLeft  = CShlOp
evalBinaryOp ShiftRight = CShrOp
evalBinaryOp BitwiseOr  = COrOp
evalBinaryOp x          = error $ "unhandled " ++ show x

evalBinary :: BinaryOp -> ExprTC -> ExprTC -> NodeInfo -> CExpr
evalBinary op e1 e2 = CBinary (evalBinaryOp op) (evalExpr e1) (evalExpr e2)

evalIntRep :: IntRep -> CIntRepr
evalIntRep Dec = DecRepr
evalIntRep Hex = HexRepr
evalIntRep Oct = OctalRepr

evalIntType :: [IntType] -> Flags CIntFlag
evalIntType = evalFlags
 where
  evalFlags []     = noFlags
  evalFlags (x:xs) = setFlag (eval x) (evalFlags xs)
  eval Unsigned = FlagUnsigned
  eval Long     = FlagLong
  eval LongLong = FlagLongLong

evalExpr :: ExprTC -> CExpr
evalExpr (LiteralTC ns (IntLiteral i rep typ)) =
  CConst (CIntConst (CInteger i (evalIntRep rep) (evalIntType typ)) $ toNI ns)
evalExpr (LiteralTC ns (StrLiteral s)) =
  CConst (CStrConst (cString $ toS s) $ toNI ns)
evalExpr (LiteralTC ns (CharLiteral c)) =
  CConst (CCharConst (cChar c) $ toNI ns)
evalExpr (BinaryTC ns op e1 e2) =
  evalBinary op e1 e2 $ toNI ns
evalExpr (FunCallTC ns (Identifier _ n) a) =
  CCall (CVar (mkIdent' n (Name 0)) un) (evalExpr <$> a) $ toNI ns
evalExpr (IdentifierTC ns n) =
  CVar (mkIdent' n (Name 0)) $ toNI ns
evalExpr (ArraySubTC ns n e) =
  CIndex (CVar (mkIdent' n (Name 0)) $ toNI ns) (evalExpr e) $ toNI ns
evalExpr (UnaryTC ns UnaryPostfix Increment e) =
  CUnary CPostIncOp (evalExpr e) $ toNI ns
evalExpr (AssignTC ns n v) =
  assign n v $ toNI ns
evalExpr (Cast ns ty e) = CCast (CDecl typ [decs'] un) (evalExpr e) $ toNI ns
 where
  (typ, decs)  = evalType ty
  decs' :: (Maybe (CDeclarator NodeInfo), Maybe a1, Maybe a2)
  decs' = (Just (CDeclr Nothing decs Nothing [] un), Nothing, Nothing)
evalExpr (MemberRefTC ns ModMem (IdentifierTC _ m) (IdentifierTC _ n)) =
  CVar (mkIdent' (m <> "$" <> n) (Name 0)) $ toNI ns
evalExpr (MemberRefTC ns ModMem (IdentifierTC _ m) (FunCallTC _ (IdentifierTC _ n) a)) =
  CCall (CVar (mkIdent' (m <> "$" <> n) (Name 0)) un) (evalExpr <$> a) $ toNI ns
evalExpr x =
  error $ "unhandled " ++ show x
