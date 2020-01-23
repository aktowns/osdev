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
import AST.Phases.Typed

import CodeGen.C.Common
import CodeGen.C.Expression
import CodeGen.C.Type

declare :: Type -> Text -> Maybe CExpr -> NodeInfo -> CDecl
declare t n v = CDecl typ [(Just (CDeclr (Just name) decs Nothing [] un), pval v, Nothing)]
 where
  name = mkIdent' n (Name 0)
  (typ, decs)  = evalType t
  pval :: Functor f => f (CExpression NodeInfo) -> f (CInitializer NodeInfo)
  pval val = val <&> \e -> CInitExpr e un

evalStmt :: StmtTC -> CBlockItem
evalStmt (DeclareTC ns n (Just t) mv) =
  CBlockDecl $ declare t n (mv <&> evalExpr) $ toNI ns
evalStmt (ReturnTC ns e) =
  CBlockStmt $ CReturn (evalExpr <$> e) $ toNI ns
evalStmt (WhileTC ns c b) =
  CBlockStmt $ CWhile (evalExpr c) (CCompound [] (fmap evalStmt b) $ toNI ns) False $ toNI ns
evalStmt (SExprTC ns e) =
  CBlockStmt $ CExpr (Just $ evalExpr e) $ toNI ns
evalStmt (ForTC ns (DeclareTC ns' n (Just t) mv) c f b) =
  CBlockStmt $
    CFor (Right $ declare t n (mv <&> evalExpr) $ toNI ns')
         (Just $ evalExpr c)
         (Just $ evalExpr f)
         (CCompound [] (fmap evalStmt b) $ toNI ns) $ toNI ns
evalStmt x =
  error $ "unhandled " ++ show x

--typedef n v =
--  CDeclExt (CDecl [CStorageSpec (CTypedef un)]
--                  [] un)
--    CDeclExt (CDecl [CStorageSpec (CTypedef),CTypeSpec (CIntType)] [(Just (CDeclr (Just (Ident "int_fast16_t" 422708096)) [] Nothing []),Nothing,Nothing)]),
