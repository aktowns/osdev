{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(foldM)

import Text.PrettyPrint (render)

import System.Environment (getArgs)

import AST
import Parser
import CodeGen.C.Pretty
import CodeGen.C.TopLevel

import Rewriters.Rewriter
import Rewriters.C.FunctionAlias
import Analyzers.Analyzer
import Analyzers.Graphviz

resolvers = []
extractors = [functionAliases]
analyzers = [graph]

evalFile :: FilePath -> IO String
evalFile fp = do
  tys <- parseFile "stdlib/types.ra"
  console <- parseFile "stdlib/console.ra"
  ast <- parseFile fp
  analyze (head analyzers) ast
  let tree = (tys ++ console ++ ast)
  -- tree <- foldM (\tree rewriter -> rewrite rewriter tree) (tys ++ console ++ ast) resolvers
  preamb <- concatMapM (\extractor -> extract extractor tree) extractors
  let ctree = evalTopLevels tree
  return $ render $ pretty $ prependC ctree preamb

main :: IO ()
main = do
  putStrLn "radon transpiler 0.00000000000001"
  args <- getArgs
  res <- case length args of
    0 -> error "filename needed"
    n -> evalFile $ head args
  putStrLn res
  writeFile "out.c" res
