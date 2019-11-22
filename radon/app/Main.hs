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

resolvers = []
extractors = [functionAliases]

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (return [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; return $ x++xs

evalFile :: FilePath -> IO String
evalFile fp = do
  tys <- parseFile "stdlib/types.ra"
  console <- parseFile "stdlib/console.ra"
  ast <- parseFile fp
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
