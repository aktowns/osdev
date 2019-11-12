{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.C.Data.Node
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.Name
import PrettyC

import Text.PrettyPrint (render)

import Data.Functor ((<&>))
import Data.Bifunctor (first, second)

import Data.Text (Text)
import qualified Data.Text as T

import System.Environment (getArgs)

import AST
import Parser

initExpr v = v <&> \e -> CInitExpr (evalExpr e) un

evalTopLevel :: [TopLevel NodeInfo] -> CTranslUnit
evalTopLevel xs = CTranslUnit (xs >>= eval) un
 where
  eval :: TopLevel NodeInfo -> [CExternalDeclaration NodeInfo]
  eval (Enum n v ni)        = [CDeclExt $ CDecl [CTypeSpec (CEnumType (enum n v) un)] [] ni]
  eval (Func n t a b ni)    = [CFDefExt $ func n t a (map evalStmt b) ni]
  eval (Decl n t me ni)     = let (typ, decs) = evalType t
                                  name = mkIdent' n (Name 0)
                              in
    [CDeclExt $ CDecl typ [(Just (CDeclr (Just name) decs Nothing [] un), initExpr me, Nothing)] ni]
  eval (Module name tls ni) = tls >>= eval -- TODO: Actually namespace the stuff

evalStmt :: Stmt NodeInfo -> CBlockItem
evalStmt (Declare n t mv ni) = declare t n (mv <&> evalExpr) ni
evalStmt (Return e ni)       = CBlockStmt $ CReturn (evalExpr <$> e) ni
evalStmt (While c b ni)      = CBlockStmt $ CWhile (evalExpr c) (CCompound [] (map evalStmt b) ni) False ni
evalStmt (SExpr e ni)        = CBlockStmt $ CExpr (Just $ evalExpr e) ni
evalStmt (For i c f b ni)    = CBlockStmt $ CFor (Left Nothing) (Just $ evalExpr c) (Just $ evalExpr f) (CCompound [] (map evalStmt b) ni) ni
evalStmt x                   = error $ "unhandled " ++ show x

evalExpr :: Expr NodeInfo -> CExpr
evalExpr (Literal (IntLiteral i) ni)         = CConst (CIntConst (cInteger i) ni)
evalExpr (Literal (StrLiteral s) ni)         = CConst (CStrConst (cString $ T.unpack s) ni)
evalExpr (Binary op e1 e2 ni)                = evalBinary op e1 e2 ni
evalExpr (FunCall n a ni)                    = CCall (CVar (mkIdent' n (Name 0)) un) (evalExpr <$> a) ni
evalExpr (Identifier n ni)                   = CVar (mkIdent' n (Name 0)) ni
evalExpr (ArraySub n e ni)                   = CIndex (CVar (mkIdent' n (Name 0)) ni) (evalExpr e) ni
evalExpr (Unary UnaryPostfix Increment e ni) = CUnary CPostIncOp (evalExpr e) ni
evalExpr (Assign n v ni)                     = assign n v ni
evalExpr x                                   = error $ "unhandled " ++ show x
 
evalBinaryOp :: BinaryOp -> CBinaryOp
evalBinaryOp Add = CAddOp
evalBinaryOp Sub = CSubOp
evalBinaryOp Mul = CMulOp
evalBinaryOp Div = CDivOp
evalBinaryOp ShiftLeft = CShlOp
evalBinaryOp ShiftRight = CShrOp
evalBinaryOp BitwiseOr = COrOp
evalBinaryOp x   = error $ "unhandled " ++ show x

evalBinary :: BinaryOp -> Expr NodeInfo -> Expr NodeInfo -> NodeInfo -> CExpr
evalBinary op e1 e2 ni = CBinary (evalBinaryOp op) (evalExpr e1) (evalExpr e2) ni

evalType :: Type -> ([CDeclarationSpecifier NodeInfo], [CDerivedDeclr])
evalType TyVoid        = ([CTypeSpec (CVoidType un)], [])
evalType TyInt         = ([CTypeSpec (CIntType un)], [])
evalType TyChar        = ([CTypeSpec (CCharType un)], [])
evalType (TyDef ty)    = ([CTypeSpec (CTypeDef (mkIdent' ty (Name 0)) un)], [])
evalType (TyPtr ty)    = second (\x -> CPtrDeclr [] un : x) $ evalType ty
evalType (TyInline ty) = first (\x -> CFunSpec (CInlineQual un) : x) $ evalType ty
evalType (TyStatic ty) = first (\x -> CStorageSpec (CStatic un) : x) $ evalType ty
evalType (TyConst ty)  = first (\x -> CTypeQual (CConstQual un) : x) $ evalType ty
evalType x             = error $ "unhandled " ++ show x

un = undefNode 

mkIdent' :: Text -> Name -> Ident
mkIdent' x = mkIdent nopos (T.unpack x)

declare t n v ni = CBlockDecl $ CDecl typ [(Just (CDeclr (Just name) decs Nothing [] un), pval v, Nothing)] ni
 where 
  name = mkIdent' n (Name 0)
  (typ, decs)  = evalType t
  pval val = val <&> \e -> CInitExpr e un

assign n v ni = 
  CAssign CAssignOp (evalExpr n) (CConst (CIntConst (cInteger 0) un)) ni

func :: Text -> Type -> [(Text, Type)] -> [CCompoundBlockItem NodeInfo] -> NodeInfo -> CFunctionDef NodeInfo
func name typ args body ni =
  CFunDef typ' (CDeclr (Just name') (args' ++ decs) Nothing [] ni) [] (CCompound [] body un) ni
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

-- app = Func "main" TyVoid [("argc", TyInt), ("argv", TyPtr (TyPtr TyChar))] [S $ Return (Just $ Literal $ IntLiteral 0)]

--typedef n v = 
--  CDeclExt (CDecl [CStorageSpec (CTypedef un)] 
--                  [] un)
--    CDeclExt (CDecl [CStorageSpec (CTypedef),CTypeSpec (CIntType)] [(Just (CDeclr (Just (Ident "int_fast16_t" 422708096)) [] Nothing []),Nothing,Nothing)]),

evalFile :: FilePath -> IO String
evalFile fp = do
  ast <- parseFile fp
  return $ render $ pretty $ evalTopLevel ast
  
main :: IO ()
main = do
  putStrLn "radon transpiler 0.00000000000001"
  args <- getArgs 
  res <- case length args of
    0 -> error "filename needed"
    n -> evalFile $ head args
  putStrLn res
  writeFile "out.c" res 
