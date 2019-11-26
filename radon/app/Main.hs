{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative

import Control.Monad(when)

import Text.PrettyPrint (render)

import Parser
import CodeGen.C.Pretty
import CodeGen.C.TopLevel

import Rewriters.Rewriter
import Rewriters.C.FunctionAlias
import Analyzers.Analyzer
import Analyzers.Graphviz

data Options = Options { targetFile :: FilePath
                       , outFile    :: FilePath
                       , verbose    :: Bool
                       } deriving (Show)

extractors :: [FunctionAliases]
extractors = [functionAliases]
analyzers :: [Graph]
analyzers = [graph]

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
 where
  f x xs = do
    x' <- op x
    if null x' then xs else do
      xs' <- xs
      pure $ x'++xs'

evalFile :: FilePath -> IO String
evalFile fp = do
  tys <- parseFile "stdlib/types.ra"
  console <- parseFile "stdlib/console.ra"
  ast <- parseFile fp
  _ <- analyze (head analyzers) ast
  let tree = tys ++ console ++ ast
  -- tree <- foldM (\tree rewriter -> rewrite rewriter tree) (tys ++ console ++ ast) resolvers
  preamb <- concatMapM (`extract` tree) extractors
  let ctree = evalTopLevels tree
  return $ render $ pretty $ prependC ctree preamb

program :: Options -> IO ()
program Options{..} = do
  res <- evalFile targetFile
  when verbose $ putStrLn res
  writeFile outFile res

options :: Parser Options
options = Options
       <$> strOption
           ( long "target"
          <> metavar "TARGET"
          <> help "target file to compile"
           )
       <*> strOption
           ( long "out"
          <> metavar "OUT"
          <> help "output file"
           )
       <*> switch
           ( long "verbose"
          <> short 'v'
          <> help "show everything"
           )

main :: IO ()
main = program =<< execParser opts
 where
  opts = info (options <**> helper)
    ( fullDesc
    <> progDesc "Compile radon files into C"
    <> header "radon - radon language compiler" )
