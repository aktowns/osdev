{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

import Language.C.Data.Node
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.Name
import Language.C.Pretty

import Text.PrettyPrint (render)

import Data.Functor ((<&>))
import Data.Bifunctor (first, second)

import Data.Text (Text)
import qualified Data.Text as T

import System.Environment (getArgs)

import AST
import Parser

evalTopLevel :: [TopLevel] -> CTranslUnit
evalTopLevel xs = CTranslUnit (map eval xs) un
 where
  eval :: TopLevel -> CExternalDeclaration NodeInfo
  eval (Enum n v)     = CDeclExt $ CDecl [CTypeSpec (CEnumType (enum n v) un)] [] un
  eval (Func n t a b) = CFDefExt $ func n t a $ evalNode b

evalNode :: [Node] -> [CBlockItem]
evalNode = map eval
 where
  eval (S stmt) = evalStmt stmt
  eval (E expr) = CBlockStmt $ CExpr (Just $ evalExpr expr) un

evalStmt :: Stmt -> CBlockItem
evalStmt (Assign n v)     = assign n v
evalStmt (Declare t n mv) = declare t n $ mv <&> evalExpr
evalStmt (Return e)       = CBlockStmt $ CReturn (evalExpr <$> e) un

evalExpr :: Expr -> CExpr
evalExpr (Literal (IntLiteral i)) = CConst (CIntConst (cInteger i) un)
evalExpr (Binary op e1 e2) = evalBinary op e1 e2
 
evalBinaryOp :: BinaryOp -> CBinaryOp
evalBinaryOp Add = CAddOp

evalBinary :: BinaryOp -> Expr -> Expr -> CExpr
evalBinary op e1 e2 = CBinary (evalBinaryOp op) (evalExpr e1) (evalExpr e2) un

evalType :: Type -> ([CDeclarationSpecifier NodeInfo], [CDerivedDeclr])
evalType TyVoid        = ([CTypeSpec (CVoidType un)], [])
evalType TyInt         = ([CTypeSpec (CIntType un)], [])
evalType TyChar        = ([CTypeSpec (CCharType un)], [])
evalType (TyPtr ty)    = second (\x -> CPtrDeclr [] un : x) $ evalType ty
evalType (TyInline ty) = first (\x -> CFunSpec (CInlineQual un) : x) $ evalType ty
evalType (TyStatic ty) = first (\x -> CStorageSpec (CStatic un) : x) $ evalType ty
evalType (TyConst ty)  = first (\x -> CTypeQual (CConstQual un) : x) $ evalType ty

un = undefNode 

mkIdent' :: Text -> Name -> Ident
mkIdent' x = mkIdent nopos (T.unpack x)

declare t n v = CBlockDecl $ CDecl typ [(Just (CDeclr (Just name) decs Nothing [] un), pval v, Nothing)] un
 where 
  name = mkIdent' n (Name 0)
  (typ, decs)  = evalType t
  pval val = val <&> \e -> CInitExpr e un

assign n v = CBlockStmt (CExpr (Just (CAssign CAssignOp (CVar (mkIdent' n (Name 0)) un) (CConst (CIntConst (cInteger 0) un)) un)) un)

func :: Text -> Type -> [(Text, Type)] -> [CCompoundBlockItem NodeInfo] -> CFunctionDef NodeInfo
func name typ args body =
  CFunDef typ' (CDeclr (Just name') (args' ++ decs) Nothing [] un) [] (CCompound [] body un) un
 where
  name' = mkIdent' name (Name 0)
  (typ', decs)  = evalType typ
  args' = [CFunDeclr (Right (evalArgs args, False)) [] un]

evalArgs :: [(Text, Type)] -> [CDeclaration NodeInfo]
evalArgs = map eval
 where
   eval (n, t) = let (t', d) = evalType t in
     CDecl t' [(Just (CDeclr (Just (mkIdent' n (Name 0))) d Nothing [] un), Nothing, Nothing)] un

enum :: Text -> [(Text, Maybe Integer)] -> CEnumeration NodeInfo
enum name xs = 
  CEnum (Just $ mkIdent' name (Name 0)) (Just $ map splat xs) [] un
 where 
   splat (n, Just v) = (mkIdent' n (Name 1), Just (CConst $ CIntConst (cInteger v) un))
   splat (n, Nothing) = (mkIdent' n (Name 1), Nothing)

--typedef n v = 
--  CDeclExt (CDecl [CStorageSpec (CTypedef un)] 
--                  [] un)
--    CDeclExt (CDecl [CStorageSpec (CTypedef),CTypeSpec (CIntType)] [(Just (CDeclr (Just (Ident "int_fast16_t" 422708096)) [] Nothing []),Nothing,Nothing)]),

evalFile :: FilePath -> IO String
evalFile fp = do
  ast <- parseFile fp
  return $ render $ pretty $ evalTopLevel [ast]  
  
