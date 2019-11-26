-----------------------------------------------------------------------------
-- |
-- Module      :  CodeGen.C.Statement
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides C code generation for radon statements
-----------------------------------------------------------------------------
module CodeGen.C.Statement where

import Language.C.Data.Name
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST

import AST
import CodeGen.C.Common
import CodeGen.C.Expression
import CodeGen.C.Type

declare :: Type -> Text -> Maybe CExpr -> NodeInfo -> CDecl
declare t n v ni = CDecl typ [(Just (CDeclr (Just name) decs Nothing [] un), pval v, Nothing)] ni
 where
  name = mkIdent' n (Name 0)
  (typ, decs)  = evalType t
  pval :: Functor f => f (CExpression NodeInfo) -> f (CInitializer NodeInfo)
  pval val = val <&> \e -> CInitExpr e un

evalStmt :: Stmt -> CBlockItem
evalStmt (Declare n t mv na) =
  CBlockDecl $ declare t n (mv <&> evalExpr) $ toNI na
evalStmt (Return e na) =
  CBlockStmt $ CReturn (evalExpr <$> e) $ toNI na
evalStmt (While c b na) =
  CBlockStmt $ CWhile (evalExpr c) (CCompound [] (fmap evalStmt b) $ toNI na) False $ toNI na
evalStmt (SExpr e na) =
  CBlockStmt $ CExpr (Just $ evalExpr e) $ toNI na
evalStmt (For (Declare n t mv na') c f b na) =
  CBlockStmt $
    CFor (Right $ declare t n (mv <&> evalExpr) $ toNI na')
         (Just $ evalExpr c)
         (Just $ evalExpr f)
         (CCompound [] (fmap evalStmt b) $ toNI na) $ toNI na
evalStmt x =
  error $ "unhandled " ++ show x

--typedef n v =
--  CDeclExt (CDecl [CStorageSpec (CTypedef un)]
--                  [] un)
--    CDeclExt (CDecl [CStorageSpec (CTypedef),CTypeSpec (CIntType)] [(Just (CDeclr (Just (Ident "int_fast16_t" 422708096)) [] Nothing []),Nothing,Nothing)]),
