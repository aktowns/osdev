{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.PrettyPrint (render)

import System.Environment (getArgs)

import AST
import Parser
import CodeGen.C.Pretty
import CodeGen.C.TopLevel

evalFile :: FilePath -> IO String
evalFile fp = do
  tls <- parseFile "stdlib/types.ra"
  ast <- parseFile fp
  return $ render $ pretty $ evalTopLevels (tls ++ ast)

main :: IO ()
main = do
  putStrLn "radon transpiler 0.00000000000001"
  args <- getArgs
  res <- case length args of
    0 -> error "filename needed"
    n -> evalFile $ head args
  putStrLn res
  writeFile "out.c" res
