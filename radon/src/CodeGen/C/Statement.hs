module CodeGen.C.Statement where

import Data.Text (Text)
import Data.Functor ((<&>))

import Language.C.Data.Name
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST

import AST
import CodeGen.C.Common
import CodeGen.C.Expression
import CodeGen.C.Type

declare :: Type -> Text -> Maybe CExpr -> NodeInfo -> CBlockItem
declare t n v ni = CBlockDecl $ CDecl typ [(Just (CDeclr (Just name) decs Nothing [] un), pval v, Nothing)] ni
 where 
  name = mkIdent' n (Name 0)
  (typ, decs)  = evalType t
  pval val = val <&> \e -> CInitExpr e un

evalStmt :: Stmt -> CBlockItem
evalStmt (Declare n t mv na) =
  declare t n (mv <&> evalExpr) $ toNI na
evalStmt (Return e na) =
  CBlockStmt $ CReturn (evalExpr <$> e) $ toNI na
evalStmt (While c b na) =
  CBlockStmt $ CWhile (evalExpr c) (CCompound [] (map evalStmt b) $ toNI na) False $ toNI na
evalStmt (SExpr e na) =
  CBlockStmt $ CExpr (Just $ evalExpr e) $ toNI na
evalStmt (For i c f b na) =
  CBlockStmt $ 
    CFor (Left Nothing) 
         (Just $ evalExpr c) 
         (Just $ evalExpr f) 
         (CCompound [] (map evalStmt b) $ toNI na) $ toNI na
evalStmt x =
  error $ "unhandled " ++ show x

--typedef n v = 
--  CDeclExt (CDecl [CStorageSpec (CTypedef un)] 
--                  [] un)
--    CDeclExt (CDecl [CStorageSpec (CTypedef),CTypeSpec (CIntType)] [(Just (CDeclr (Just (Ident "int_fast16_t" 422708096)) [] Nothing []),Nothing,Nothing)]),
