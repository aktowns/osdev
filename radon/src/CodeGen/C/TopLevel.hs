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
import CodeGen.C.Common
import CodeGen.C.Expression
import CodeGen.C.Statement
import CodeGen.C.Type

initExpr :: Maybe Expr -> Maybe CInit
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

typedef :: Type -> Text -> NodeAnnotation -> CDecl
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

evalTopLevel :: Maybe Text -> TL -> [CExtDecl]
evalTopLevel pfx (Enum n v na) =
  [CDeclExt $ CDecl [
      CStorageSpec (CTypedef un),CTypeSpec (CEnumType (enum (prefix pfx n) v) un)
    ] [(Just (CDeclr (Just (mkIdent' n (Name 0))) [] Nothing [] un), Nothing, Nothing)] $ toNI na]
evalTopLevel pfx (Func n t a b na) =
  [CFDefExt $ func (prefix pfx n) t a (fmap evalStmt b) $ toNI na]
evalTopLevel pfx (Decl n t me na) =
  [CDeclExt $ CDecl typ [(Just (CDeclr (Just name) decs Nothing [] un), initExpr me, Nothing)] $ toNI na]
 where
   (typ, decs) = evalType t
   name = mkIdent' (prefix pfx n) (Name 0)
evalTopLevel pfx (Struct n fields na) = [CDeclExt $ struct (prefix pfx n) fields na True]
evalTopLevel pfx (Union n cons na)    = CDeclExt <$> union (prefix pfx n) cons na
evalTopLevel _ (Module name tls _) = tls >>= evalTopLevel (Just name)
evalTopLevel pfx (TypeDef name ty na) = [CDeclExt $ typedef ty (prefix pfx name) na]
evalTopLevel _ Import {} = [] -- TODO: handle
evalTopLevel pfx (Alias (Just C) (retTy, from, args) to na) = [CDeclExt $ alias retTy from args (prefix pfx to) na]
evalTopLevel _ x = error $ "unhandled: " ++ show x

evalTopLevels :: [TL] -> CTranslUnit
evalTopLevels xs = CTranslUnit (xs >>= evalTopLevel Nothing) un

prependC :: CTranslUnit -> [CExtDecl] -> CTranslUnit
prependC (CTranslUnit decs na) xs = CTranslUnit (xs ++ decs) na

evalAliasArgs :: [Type] -> [CDecl]
evalAliasArgs = fmap eval
 where
   eval t = let (t', d) = evalType t in
     CDecl t' [(Just (CDeclr Nothing d Nothing [] un), Nothing, Nothing)] un

alias :: Type -> Text -> ([Type], Bool) -> Text -> NodeAnnotation -> CDecl
alias retTy name (args, vararg) newName na = CDecl retTyp [ (Just (CDeclr (Just name') (args' ++ retDecs) (Just old') [] un), Nothing, Nothing)] $ toNI na
 where
  (retTyp, retDecs) = evalType retTy
  name' = mkIdent' newName (Name 0)
  old' = CStrLit (cString $ toS name) un
  args' = [CFunDeclr (Right (evalAliasArgs args, vararg)) [] un]

struct :: Text -> [(Text, Type)] -> NodeAnnotation -> Bool -> CDeclaration NodeInfo
struct n f na td = CDecl (tydef ++ [ CTypeSpec (CSUType (CStruct CStructTag Nothing (Just $ fmap field f) [] un) un)
                         ]) [ (Just (CDeclr (Just $ mkIdent' n (Name 0)) [] Nothing [] un), Nothing, Nothing) ] $ toNI na
 where
  tydef = [CStorageSpec (CTypedef un) | td]
  field (n', t) = CDecl ty [(Just $ CDeclr (Just $ mkIdent' n' (Name 0)) tyDecs Nothing [] un, Nothing, Nothing)] un
   where
    (ty, tyDecs) = evalType t


union :: Text -> [(Text, [(Text, Type)])] -> NodeAnnotation -> [CDeclaration NodeInfo]
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
