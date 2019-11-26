-----------------------------------------------------------------------------
-- |
-- Module      : Analyzers.Graphviz
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- Generates dot graphs of the syntax tree
-----------------------------------------------------------------------------
module Analyzers.Graphviz where

import qualified Dot as D
import qualified Data.Text as T
import Control.Monad (replicateM)

import System.Random

import AST
import Analyzers.Analyzer

data Graph = GraphCon

graph :: Graph
graph = GraphCon

instance Analyzer Graph where
  analyze _ tree = do
    ptree <- concatMapM (fmap snd . ppTL) tree
    let gviz = D.DotGraph D.Strict D.Directed (Just "ast") ptree
    D.encodeToFile "out.dot" gviz
    return Ok

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (return [])
 where
  f x xs = do
    x' <- op x
    if null x' then xs else do
      xs' <- xs
      return $ x'++xs'

freshId :: IO D.NodeId
freshId = do
  str <- T.pack <$> replicateM 12 (randomRIO ('a', 'z'))
  return $ D.NodeId (D.Id str) Nothing

stmt :: Text -> D.NodeId -> D.Statement
stmt n i = D.StatementNode $ D.NodeStatement i [ D.Attribute "label" $ D.Id n
                                               , D.Attribute "fontsize" "10"
                                               , D.Attribute "fixedsize" "true"
                                               , D.Attribute "width" "1.0"
                                               ]

nstmt :: Text -> D.NodeId -> D.Statement
nstmt n i = D.StatementNode $ D.NodeStatement i [ D.Attribute "label" $ D.Id n
                                                , D.Attribute "color" "blue"
                                                , D.Attribute "shape" "box"
                                                , D.Attribute "fontsize" "9"
                                                , D.Attribute "fixedsize" "true"
                                                , D.Attribute "width" "0.9"
                                                ]

gstmt :: Text -> D.NodeId -> D.Statement
gstmt n i = D.StatementNode $ D.NodeStatement i [ D.Attribute "label" $ D.Id n
                                                , D.Attribute "color" "red"
                                                , D.Attribute "shape" "diamond"
                                                , D.Attribute "fontsize" "8"
                                                , D.Attribute "fixedsize" "true"
                                                , D.Attribute "width" "0.6"
                                                ]

edge :: D.NodeId -> D.NodeId -> D.Statement
edge n1 n2 = D.StatementEdge $ D.EdgeStatement (D.ListTwo (D.EdgeNode n1) (D.EdgeNode n2) []) []

nedge :: Text -> D.NodeId -> D.NodeId -> D.Statement
nedge n n1 n2 =
  D.StatementEdge $ D.EdgeStatement (D.ListTwo (D.EdgeNode n1) (D.EdgeNode n2) [])
  [ D.Attribute "xlabel" $ D.Id ("   " <> n <> "   ")
  , D.Attribute "fontsize" "8"
  ]

sub :: Text -> D.NodeId -> [D.Statement] -> D.Statement
sub n (D.NodeId i _) xs =
  D.StatementSubgraph $ D.Subgraph (Just i) $
    D.StatementAttribute (D.AttributeStatement D.Graph [D.Attribute "label" $ D.Id n]) : xs

ppStmt :: Statement a -> IO (D.NodeId, [D.Statement])
ppStmt (SExpr e _)  = do
  i <- freshId
  (j, expr) <- ppExpr e

  return (i, [ nstmt "stmt expr" i
             , edge i j
             ] ++ expr)
ppStmt (Return (Just expr) _) = do
  i <- freshId
  (j, expr') <- ppExpr expr

  return (i, [ nstmt "return" i
             , edge i j
             ] ++ expr')
ppStmt (Return Nothing _) = do
  i <- freshId

  return (i, [ nstmt "return" i ])
ppStmt (Declare n t Nothing _) = do
  i <- freshId
  j <- freshId
  (k, ty) <- ppType t

  return (i, [ nstmt "declare" i
             , stmt n j
             , nedge "identifier" i j
             , nedge "type" i k
             ] ++ ty)
ppStmt (Declare n t (Just e) _) = do
  i <- freshId
  j <- freshId
  (k, ty) <- ppType t
  (l, ex) <- ppExpr e

  return (i, [ nstmt "declare" i
             , stmt n j
             , nedge "identifier" i j
             , nedge "type" i k
             , nedge "expr" i l
             ] ++ ty ++ ex)
ppStmt (While e s _) = do
  i <- freshId
  (j, ex) <- ppExpr e
  k <- freshId
  (links, body) <- unzip <$> mapM ppStmt s

  return (i, [ nstmt "while" i
             , nedge "expr" i j
             , gstmt "stmts" k
             ] ++ ex ++ (edge k <$> links) ++ concat body)

ppLit :: Lit -> IO (D.NodeId, [D.Statement])
ppLit (IntLiteral n rep typ) = do
  i <- freshId
  j <- freshId

  return (i, [ nstmt "int literal" i
             , stmt (T.pack $ show n) j
             , edge i j
             ])
ppLit (StrLiteral n) = do
  i <- freshId
  j <- freshId

  return (i, [ nstmt "string literal" i
             , stmt (T.pack $ show n) j
             , edge i j
             ])
ppLit (CharLiteral n) = do
  i <- freshId
  j <- freshId

  return (i, [ nstmt "char literal" i
             , stmt (T.pack $ show n) j
             , edge i j
             ])

ppType :: Type -> IO (D.NodeId, [D.Statement])
ppType TyVoid = do
  i <- freshId
  return (i, [ nstmt "TyVoid" i ])

ppType (TyDef ty)    = do
  i <- freshId
  j <- freshId
  return (i, [ nstmt "TyDef" i
             , stmt ty j
             , edge i j
             ])
ppType (TyPtr ty) = do
  i <- freshId
  (j, ty') <- ppType ty

  return (i, [ nstmt "TyPtr" i
             , edge i j ] ++ ty')
ppType (TyStatic ty) = do
  i <- freshId
  (j, ty') <- ppType ty

  return (i, [ nstmt "TyStatic" i
             , edge i j ] ++ ty')
ppType (TyInline ty) = do
  i <- freshId
  (j, ty') <- ppType ty

  return (i, [ nstmt "TyInline" i
             , edge i j ] ++ ty')
ppType (TyConst ty) = do
  i <- freshId
  (j, ty') <- ppType ty

  return (i, [ nstmt "TyConst" i
             , edge i j ] ++ ty')

ppExpr :: Expression a -> IO (D.NodeId, [D.Statement])
ppExpr (Literal lit' _) = do
  i <- freshId
  (j, lit) <- ppLit lit'

  return (i, [ nstmt "literal" i
             , edge i j
             ] ++ lit)
ppExpr (FunCall n a _) = do
  i <- freshId
  j <- freshId
  (links, args) <- unzip <$> mapM ppExpr a
  k <- freshId

  return (i, [ nstmt "function call" i
             , stmt n j
             , nedge "identifier" i j
             , gstmt "args" k
             , nedge "args" i k
             ] ++ (edge k <$> links) ++ concat args)
ppExpr (MemberRef ModMem (Identifier m _) expr _) = do
  i <- freshId
  (j, expr') <- ppExpr expr

  return (i, [ stmt m i
             , edge i j
             ] ++ expr')

ppArgs :: ([(Text, Type)], Bool) -> IO (D.NodeId, [D.Statement])
ppArgs (xs, var) = do
  i <- freshId
  (links, decls) <- unzip <$> mapM ppArg xs

  return (i, gstmt "args" i : (edge i <$> links) ++ concat decls)
 where
  ppArg (n, t) = do
    i <- freshId
    j <- freshId
    (k, typ) <- ppType t

    return (i, [ gstmt "arg" i
               , stmt n j
               , nedge "identifier" i j
               , nedge "type" i k
               ] ++ typ)

ppStructFields :: [(Text, Type)] -> IO (D.NodeId, [D.Statement])
ppStructFields xs = do
  i <- freshId
  (links, decls) <- unzip <$> mapM ppField xs

  return (i, gstmt "fields" i : (edge i <$> links) ++ concat decls)
 where
  ppField (n, t) = do
    i <- freshId
    j <- freshId
    (k, typ) <- ppType t

    return (i, [ gstmt "field" i
               , stmt n j
               , nedge "identifier" i j
               , nedge "type" i k
               ] ++ typ)

ppUnionCons :: [(Text, [(Text, Type)])] -> IO (D.NodeId, [D.Statement])
ppUnionCons xs = do
  i <- freshId
  (links, decls) <- unzip <$> mapM ppUnionCon xs

  return (i, gstmt "cons" i : (edge i <$> links) ++ concat decls)
 where
  ppUnionCon (n, xs') = do
    i <- freshId
    j <- freshId
    (links, decls) <- unzip <$> mapM ppField xs'

    return (i, [ gstmt "fields" i
               , stmt n j
               , nedge "name" i j
               ] ++ (edge i <$> links) ++ concat decls)
  ppField (n, t) = do
    i <- freshId
    j <- freshId
    (k, typ) <- ppType t

    return (i, [ gstmt "field" i
               , stmt n j
               , nedge "identifier" i j
               , nedge "type" i k
               ] ++ typ)

ppTL :: TopLevel a -> IO (D.NodeId, [D.Statement])
ppTL (Union n cons _) = do
  i <- freshId
  j <- freshId
  (k, cons') <- ppUnionCons cons

  return (i, [ nstmt "union" i
             , stmt n j
             , nedge "name" i j
             , nedge "cons" i k
             ] ++ cons')
ppTL (Struct n fields _) = do
  i <- freshId
  j <- freshId
  (k, fields') <- ppStructFields fields

  return (i, [ nstmt "struct" i
             , stmt n j
             , nedge "name" i j
             , nedge "fields" i k
             ] ++ fields')
ppTL (Func name retTy args body _) = do
  i <- freshId
  j <- freshId
  (k, typ) <- ppType retTy
  (l, argg) <- ppArgs args
  (links, body') <- unzip <$> mapM ppStmt body
  m <- freshId

  return (i, [ nstmt "function" i
             , stmt name j
             , nedge "name" i j
             , nedge "type" i k
             , nedge "args" i l
             , gstmt "body" m
             , edge i m
             ] ++ typ ++ argg ++ (edge m <$> links) ++ concat body')
ppTL (Module name body _) = do
  i <- freshId
  j <- freshId
  (links, body') <- unzip <$> mapM ppTL body
  m <- freshId

  return (i, [ nstmt "module" i
             , stmt name j
             , gstmt "body" m
             , edge i m
             ] ++ (edge m <$> links) ++ concat body')
