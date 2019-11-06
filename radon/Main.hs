module Main where

import Language.C.Data.Node
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.Name

import Data.Functor ((<&>))

data Type = TyVoid 
          | TyStatic Type
          | TyInline Type
          | TyConst Type
          deriving (Show)

data BinaryOp = Add deriving Show

data TopLevel = Enum String [(String, Maybe Integer)]
              | Func String Type [(String, Type)] [Node]
              deriving (Show)

data Lit = IntLiteral Integer deriving (Show)

data Expr = Literal Lit
          | Binary BinaryOp Expr Expr
          deriving (Show)

data Stmt = Assign String Node
          | Declare Type String (Maybe Expr)
          deriving (Show)

data Node = E Expr
          | S Stmt
          deriving (Show)

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

evalExpr :: Expr -> CExpr
evalExpr (Literal (IntLiteral i)) = CConst (CIntConst (cInteger i) un)
evalExpr (Binary op e1 e2) = evalBinary op e1 e2
 
evalBinaryOp :: BinaryOp -> CBinaryOp
evalBinaryOp Add = CAddOp

evalBinary :: BinaryOp -> Expr -> Expr -> CExpr
evalBinary op e1 e2 = CBinary (evalBinaryOp op) (evalExpr e1) (evalExpr e2) un

evalType :: Type -> [CDeclarationSpecifier NodeInfo]
evalType TyVoid        = [CTypeSpec (CVoidType un)]
evalType (TyInline ty) = CFunSpec (CInlineQual un) : evalType ty
evalType (TyStatic ty) = CStorageSpec (CStatic un) : evalType ty
evalType (TyConst ty)  = CTypeQual (CConstQual un) : evalType ty

un = undefNode 

mkIdent' = mkIdent nopos

declare t n v = CBlockDecl $ CDecl typ [(Just (CDeclr (Just name) [] Nothing [] un), pval v, Nothing)] un
 where 
  name = mkIdent' n (Name 0)
  typ  = evalType t
  pval val = val <&> \e -> CInitExpr e un

assign n v = CBlockStmt (CExpr (Just (CAssign CAssignOp (CVar (mkIdent' n (Name 0)) un) (CConst (CIntConst (cInteger 0) un)) un)) un)

func :: String -> Type -> [(String, Type)] -> [CCompoundBlockItem NodeInfo] -> CFunctionDef NodeInfo
func name typ args body =
  CFunDef typ' (CDeclr (Just name') args' Nothing [] un) [] (CCompound [] body un) un
 where
  name' = mkIdent' name (Name 0)
  typ'  = evalType typ
  args' = [CFunDeclr (Right (evalArgs args, False)) [] un]

evalArgs :: [(String, Type)] -> [CDeclaration NodeInfo]
evalArgs = map eval
 where
   eval (n, t) = 
     CDecl (evalType t) [(Just (CDeclr (Just (mkIdent' n (Name 0))) [] Nothing [] un), Nothing, Nothing)] un

enum :: String -> [(String, Maybe Integer)] -> CEnumeration NodeInfo
enum name xs = 
  CEnum (Just $ mkIdent' name (Name 0)) (Just $ map splat xs) [] un
 where 
   splat (n, Just v) = (mkIdent' n (Name 1), Just (CConst $ CIntConst (cInteger v) un))
   splat (n, Nothing) = (mkIdent' n (Name 1), Nothing)

--typedef n v = 
--  CDeclExt (CDecl [CStorageSpec (CTypedef un)] 
--                  [] un)
--    CDeclExt (CDecl [CStorageSpec (CTypedef),CTypeSpec (CIntType)] [(Just (CDeclr (Just (Ident "int_fast16_t" 422708096)) [] Nothing []),Nothing,Nothing)]),

main :: IO ()
main = putStrLn "Hello, Haskell!"
