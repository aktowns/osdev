{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(foldM)

import Text.PrettyPrint (render)

import System.Environment (getArgs)

import AST
import Parser
import CodeGen.C.Pretty
import CodeGen.C.TopLevel

import Resolvers.Resolver
import Resolvers.C.FunctionAlias

resolvers = [functionAliases]

evalFile :: FilePath -> IO String
evalFile fp = do
  tys <- parseFile "stdlib/types.ra"
  console <- parseFile "stdlib/console.ra"
  ast <- parseFile fp
  tree <- foldM (\tree resolver -> resolve resolver tree) (tys ++ console ++ ast) resolvers
  return $ render $ pretty $ evalTopLevels tree

main :: IO ()
main = do
  putStrLn "radon transpiler 0.00000000000001"
  args <- getArgs
  res <- case length args of
    0 -> error "filename needed"
    n -> evalFile $ head args
  putStrLn res
  writeFile "out.c" res
