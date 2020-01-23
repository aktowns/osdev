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

import Data.String (IsString)

import Language.C.Data.Name
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node (NodeInfo)

import AST
import AST.Phases.Typed

import CodeGen.C.Common
import CodeGen.C.Expression
import CodeGen.C.Statement
import CodeGen.C.Type

initExpr :: Maybe ExprTC -> Maybe CInit
initExpr v = v <&> \e -> CInitExpr (evalExpr e) un

evalArgs :: [(Text, Type)] -> [CDecl]
evalArgs = fmap eval
 where
   eval (n, t) = let (t', d) = evalType t in
     CDecl t' [(Just (CDeclr (Just (mkIdent' n (Name 0))) d Nothing [] un), Nothing, Nothing)] un

enum :: Text -> [(Text, Maybe Integer)] -> CEnum
enum name xs =
  CEnum (Just $ mkIdent' name (Name 0)) (Just $ fmap splat xs) [] un
 where
   splat (n, Just v) = (mkIdent' (name <> "$" <> n) (Name 1), Just (CConst $ CIntConst (cInteger v) un))
   splat (n, Nothing) = (mkIdent' (name <> "$" <> n) (Name 1), Nothing)

typedef :: Type -> Text -> NodeSource -> CDecl
typedef ty name na = CDecl (CStorageSpec (CTypedef un) : t')
    [(Just (CDeclr (Just (mkIdent' name (Name 0))) d Nothing [] un), Nothing, Nothing)] $ toNI na
 where (t', d) = evalType ty

func :: Text -> Type -> ([(Text, Type)], Bool) -> [CBlockItem] -> NodeInfo -> CFunDef
func name typ (args, vararg) body ni =
  CFunDef typ' (CDeclr (Just name') (args' ++ decs) Nothing [] ni) [] (CCompound [] body un) ni
 where
  name' = mkIdent' name (Name 0)
  (typ', decs)  = evalType typ
  args' = [CFunDeclr (Right (evalArgs args, False)) [] un]

prefix :: (Semigroup a, IsString a) => Maybe a -> a -> a
prefix (Just x) y = x <> "$" <> y
prefix _ y = y

evalTopLevel :: Maybe Text -> ToplTC -> [CExtDecl]
evalTopLevel pfx (Enum ns n v) =
  [CDeclExt $ CDecl [
      CStorageSpec (CTypedef un),CTypeSpec (CEnumType (enum (prefix pfx n) v) un)
    ] [(Just (CDeclr (Just (mkIdent' n (Name 0))) [] Nothing [] un), Nothing, Nothing)] $ toNI ns]
evalTopLevel pfx (Func ns n t a b) =
  [CFDefExt $ func (prefix pfx n) t a (fmap evalStmt b) $ toNI ns]
evalTopLevel pfx (Decl ns n t me) =
  [CDeclExt $ CDecl typ [(Just (CDeclr (Just name) decs Nothing [] un), initExpr me, Nothing)] $ toNI ns]
 where
   (typ, decs) = evalType t
   name = mkIdent' (prefix pfx n) (Name 0)
evalTopLevel pfx (Struct ns n fields) = [CDeclExt $ struct (prefix pfx n) fields ns True]
evalTopLevel pfx (Union ns n cons)    = CDeclExt <$> union (prefix pfx n) cons ns
evalTopLevel _ (Module _ name tls) = tls >>= evalTopLevel (Just name)
evalTopLevel pfx (TypeDef ns name ty) = [CDeclExt $ typedef ty (prefix pfx name) ns]
evalTopLevel _ Import {} = [] -- TODO: handle
evalTopLevel pfx (Alias ns (Just C) (retTy, from, args) to) = [CDeclExt $ alias retTy from args (prefix pfx to) ns]

evalTopLevels :: [ToplTC] -> CTranslUnit
evalTopLevels xs = CTranslUnit (xs >>= evalTopLevel Nothing) un

prependC :: CTranslUnit -> [CExtDecl] -> CTranslUnit
prependC (CTranslUnit decs na) xs = CTranslUnit (xs ++ decs) na

evalAliasArgs :: [Type] -> [CDecl]
evalAliasArgs = fmap eval
 where
   eval t = let (t', d) = evalType t in
     CDecl t' [(Just (CDeclr Nothing d Nothing [] un), Nothing, Nothing)] un

alias :: Type -> Text -> ([Type], Bool) -> Text -> NodeSource -> CDecl
alias retTy name (args, vararg) newName na = CDecl retTyp [ (Just (CDeclr (Just name') (args' ++ retDecs) (Just old') [] un), Nothing, Nothing)] $ toNI na
 where
  (retTyp, retDecs) = evalType retTy
  name' = mkIdent' newName (Name 0)
  old' = CStrLit (cString $ toS name) un
  args' = [CFunDeclr (Right (evalAliasArgs args, vararg)) [] un]

struct :: Text -> [(Text, Type)] -> NodeSource -> Bool -> CDeclaration NodeInfo
struct n f na td = CDecl (tydef ++ [ CTypeSpec (CSUType (CStruct CStructTag Nothing (Just $ fmap field f) [] un) un)
                         ]) [ (Just (CDeclr (Just $ mkIdent' n (Name 0)) [] Nothing [] un), Nothing, Nothing) ] $ toNI na
 where
  tydef = [CStorageSpec (CTypedef un) | td]
  field (n', t) = CDecl ty [(Just $ CDeclr (Just $ mkIdent' n' (Name 0)) tyDecs Nothing [] un, Nothing, Nothing)] un
   where
    (ty, tyDecs) = evalType t


union :: Text -> [(Text, [(Text, Type)])] -> NodeSource -> [CDeclaration NodeInfo]
union n c na =
  [ CDecl [ CStorageSpec (CTypedef un)
          , CTypeSpec (CEnumType (enum (prefix (Just n) "tag") ucons) un)
          ] [(Just (CDeclr (Just (mkIdent' (prefix (Just n) "tag") (Name 0))) [] Nothing [] un), Nothing, Nothing)] $ toNI na
  , CDecl [ CStorageSpec (CTypedef un)
          , CTypeSpec (CSUType (CStruct CStructTag Nothing (Just [tag, structunion $ fmap cons c]) [] un) un)
          ] [ (Just (CDeclr (Just $ mkIdent' n (Name 0)) [] Nothing [] un), Nothing, Nothing) ] $ toNI na
  ]
  where
    structunion xs = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just xs) [] un) un)] [
      (Just (CDeclr (Just $ mkIdent' "_$value" (Name 0)) [] Nothing [] un), Nothing, Nothing)] un
    ucons :: [(Text, Maybe Integer)]
    ucons = (\x -> (fst x, Nothing)) <$> c
    cons (n', fields) = struct n' fields na False
    tag = CDecl [CTypeSpec (CTypeDef (mkIdent' (prefix (Just n) "tag") (Name 0)) un)]
                [(Just (CDeclr (Just (mkIdent' "_$tag" (Name 0))) [] Nothing [] un), Nothing, Nothing)] un
