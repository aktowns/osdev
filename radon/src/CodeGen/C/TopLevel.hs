-----------------------------------------------------------------------------
-- |
-- Module      :  CodeGen.C.TopLevel
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides C code generation for top level radon nodes
-----------------------------------------------------------------------------
module CodeGen.C.TopLevel where

import Data.Functor ((<&>))
import Data.Text (Text)

import Language.C.Data.Name
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node (NodeInfo)

import AST
import CodeGen.C.Common
import CodeGen.C.Expression
import CodeGen.C.Statement
import CodeGen.C.Type

initExpr :: Maybe Expr -> Maybe CInit
initExpr v = v <&> \e -> CInitExpr (evalExpr e) un

evalArgs :: [(Text, Type)] -> [CDecl]
evalArgs = map eval
 where
   eval (n, t) = let (t', d) = evalType t in
     CDecl t' [(Just (CDeclr (Just (mkIdent' n (Name 0))) d Nothing [] un), Nothing, Nothing)] un

enum :: Text -> [(Text, Maybe Integer)] -> CEnum
enum name xs =
  CEnum (Just $ mkIdent' name (Name 0)) (Just $ map splat xs) [] un
 where
   splat (n, Just v) = (mkIdent' n (Name 1), Just (CConst $ CIntConst (cInteger v) un))
   splat (n, Nothing) = (mkIdent' n (Name 1), Nothing)

func :: Text -> Type -> [(Text, Type)] -> [CBlockItem] -> NodeInfo -> CFunDef
func name typ args body ni =
  CFunDef typ' (CDeclr (Just name') (args' ++ decs) Nothing [] ni) [] (CCompound [] body un) ni
 where
  name' = mkIdent' name (Name 0)
  (typ', decs)  = evalType typ
  args' = [CFunDeclr (Right (evalArgs args, False)) [] un]

evalTopLevel :: TL -> [CExtDecl]
evalTopLevel (Enum n v na) =
  [CDeclExt $ CDecl [CTypeSpec (CEnumType (enum n v) un)] [] $ toNI na]
evalTopLevel (Func n t a b na) =
  [CFDefExt $ func n t a (map evalStmt b) $ toNI na]
evalTopLevel (Decl n t me na) =
  [CDeclExt $ CDecl typ [(Just (CDeclr (Just name) decs Nothing [] un), initExpr me, Nothing)] $ toNI na]
 where
   (typ, decs) = evalType t
   name = mkIdent' n (Name 0)
evalTopLevel (Module name tls na) = tls >>= evalTopLevel -- TODO: Actually namespace the stuff

evalTopLevels :: [TL] -> CTranslUnit
evalTopLevels xs = CTranslUnit (xs >>= evalTopLevel) un
