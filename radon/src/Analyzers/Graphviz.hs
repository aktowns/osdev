module Analyzers.Graphviz where

import qualified Dot as D
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; return $ x++xs

freshId = do
  str <- T.pack <$> replicateM 12 (randomRIO ('a', 'z'))
  return $ D.NodeId (D.Id str) Nothing

stmt n i = D.StatementNode $ D.NodeStatement i [ D.Attribute "label" $ D.Id n
                                               , D.Attribute "fontsize" "10"
                                               , D.Attribute "fixedsize" "true"
                                               , D.Attribute "width" "1.0"
                                               ]
nstmt n i = D.StatementNode $ D.NodeStatement i [ D.Attribute "label" $ D.Id n
                                                , D.Attribute "color" "blue"
                                                , D.Attribute "shape" "box"
                                                , D.Attribute "fontsize" "9"
                                                , D.Attribute "fixedsize" "true"
                                                , D.Attribute "width" "0.9"
                                                ]
gstmt n i = D.StatementNode $ D.NodeStatement i [ D.Attribute "label" $ D.Id n
                                                , D.Attribute "color" "red"
                                                , D.Attribute "shape" "diamond"
                                                , D.Attribute "fontsize" "8"
                                                , D.Attribute "fixedsize" "true"
                                                , D.Attribute "width" "0.6"
                                                ]
edge n1 n2 = D.StatementEdge $ D.EdgeStatement (D.ListTwo (D.EdgeNode n1) (D.EdgeNode n2) []) []
nedge n n1 n2 =
  D.StatementEdge $ D.EdgeStatement (D.ListTwo (D.EdgeNode n1) (D.EdgeNode n2) [])
  [ D.Attribute "xlabel" $ D.Id ("   " <> n <> "   ")
  , D.Attribute "fontsize" "8"
  ]

sub :: Text -> D.NodeId -> [D.Statement] -> D.Statement
sub n (D.NodeId i _) xs =
  D.StatementSubgraph $ D.Subgraph (Just i) $
    [D.StatementAttribute $ D.AttributeStatement D.Graph [D.Attribute "label" $ D.Id n]] ++ xs

ppStmt (SExpr e _)  = do
  i <- freshId
  (j, expr) <- ppExpr e

  return (i, [ nstmt "stmt expr" i
             , edge i j
             ] ++ expr)
ppStmt (Return (Just expr) _) = do
  i <- freshId
  (j, expr) <- ppExpr expr

  return (i, [ nstmt "return" i
             , edge i j
             ] ++ expr)


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

ppType (TyDef ty)    = do
  i <- freshId
  j <- freshId
  return (i, [ nstmt "TyDef" i
             , stmt ty j
             , edge i j
             ])

ppExpr (Literal lit _) = do
  i <- freshId
  (j, lit) <- ppLit lit

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

ppUnionCons xs = do
  i <- freshId
  (links, decls) <- unzip <$> mapM ppUnionCon xs

  return (i, gstmt "cons" i : (edge i <$> links) ++ concat decls)
 where
  ppUnionCon (n, xs) = do
    i <- freshId
    j <- freshId
    (links, decls) <- unzip <$> mapM ppField xs

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

-- Union Text [(Text, [(Text, Type)])] a
-- Struct Text [(Text, Type)] a
ppTL (Union n cons _) = do
  i <- freshId
  j <- freshId
  (k, cons) <- ppUnionCons cons

  return (i, [ nstmt "union" i
             , stmt n j
             , nedge "name" i j
             , nedge "cons" i k
             ] ++ cons)
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
  (links, body) <- unzip <$> mapM ppStmt body
  m <- freshId

  a <- freshId

  return (i, [ nstmt "function" i
             , stmt name j
             , nedge "name" i j
             , nedge "type" i k
             , nedge "args" i l
             , gstmt "body" m
             , edge i m
             ] ++ typ ++ argg ++ (edge m <$> links) ++ concat body)
