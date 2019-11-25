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

import qualified Data.Text as T

import Language.C.Data.Name
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants

import AST
import CodeGen.C.Common
import CodeGen.C.Type

assign :: Expr -> Expr -> NodeInfo -> CExpr
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

evalBinary :: BinaryOp -> Expr -> Expr -> NodeInfo -> CExpr
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

evalExpr :: Expr -> CExpr
evalExpr (Literal (IntLiteral i rep typ) na) =
  CConst (CIntConst (CInteger i (evalIntRep rep) (evalIntType typ)) $ toNI na)
evalExpr (Literal (StrLiteral s) na) =
  CConst (CStrConst (cString $ T.unpack s) $ toNI na)
evalExpr (Literal (CharLiteral c) na) =
  CConst (CCharConst (cChar c) $ toNI na)
evalExpr (Binary op e1 e2 na) =
  evalBinary op e1 e2 $ toNI na
evalExpr (FunCall n a na) =
  CCall (CVar (mkIdent' n (Name 0)) un) (evalExpr <$> a) $ toNI na
evalExpr (Identifier n na) =
  CVar (mkIdent' n (Name 0)) $ toNI na
evalExpr (ArraySub n e na) =
  CIndex (CVar (mkIdent' n (Name 0)) $ toNI na) (evalExpr e) $ toNI na
evalExpr (Unary UnaryPostfix Increment e na) =
  CUnary CPostIncOp (evalExpr e) $ toNI na
evalExpr (Assign n v na) =
  assign n v $ toNI na
evalExpr (Cast ty e na) = CCast (CDecl typ [decs'] un) (evalExpr e) $ toNI na
 where
  (typ, decs)  = evalType ty
  decs' :: (Maybe (CDeclarator NodeInfo), Maybe a1, Maybe a2)
  decs' = (Just (CDeclr Nothing decs Nothing [] un), Nothing, Nothing)
evalExpr (MemberRef ModMem (Identifier m _) (Identifier n _) na) =
  CVar (mkIdent' (m <> "$" <> n) (Name 0)) $ toNI na
evalExpr (MemberRef ModMem (Identifier m _) (FunCall n a _) na) =
  CCall (CVar (mkIdent' (m <> "$" <> n) (Name 0)) un) (evalExpr <$> a) $ toNI na
evalExpr x =
  error $ "unhandled " ++ show x
