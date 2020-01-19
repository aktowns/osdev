{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, GADTs #-}
module Main where

import Options.Applicative
import System.Console.Haskeline

import Control.Monad(when)

import Text.PrettyPrint (render)
import Parser
import qualified Parser.Common as PC
import Parser.TopLevel (pTopLevel)
import Parser.Statement (pStmt)
import CodeGen.C.Pretty
import CodeGen.C.TopLevel

import AST.Phases.Undecorated

import Rewriters.Rewriter
import Rewriters.C.FunctionAlias
import Analyzers.Analyzer
import Analyzers.Graphviz

data Options = Options { verbose    :: Bool
                       } deriving (Show)

extractors :: [FunctionAliases]
extractors = [functionAliases]

analyzers :: [Graph]
analyzers = [graph]

-- concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- {-# INLINE concatMapM #-}
-- concatMapM op = foldr f (pure [])
--  where
--   f x xs = do
--     x' <- op x
--     if null x' then xs else do
--       xs' <- xs
--       pure $ x'++xs'

-- evalFile :: FilePath -> IO String
-- evalFile fp = do
--   tys <- parseFile "stdlib/types.ra"
--   console <- parseFile "stdlib/console.ra"
--   ast <- parseFile fp
--   _ <- analyze (head analyzers) (toUndecorated <$> ast)
--   let tree = tys ++ console ++ ast
--   -- tree <- foldM (\tree rewriter -> rewrite rewriter tree) (tys ++ console ++ ast) resolvers
--   preamb <- concatMapM (`extract` tree) extractors
--   let ctree = evalTopLevels tree
--   return $ render $ pretty $ prependC ctree preamb

evalShowable :: [PC.Parser Text] -> Text -> Text
evalShowable [] _       = "no parsers given.."
evalShowable (x:[]) inp = bifold $ parseText x "<input>" inp
evalShowable (x:xs) inp = bifoldMap (\_ -> evalShowable xs inp) id $ parseText x "<input>" inp

program :: Options -> IO ()
program Options{..} = runInputT defaultSettings loop
 where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "% "
    case minput of
      Nothing -> pure ()
      Just "quit" -> pure ()
      Just "" -> loop
      Just input -> do
        outputStrLn (toS $ evalShowable [PC.showParser pTopLevel, PC.showParser pStmt] $ toS input)
        loop

options :: Parser Options
options = Options
       <$> switch
           ( long "verbose"
          <> short 'v'
          <> help "show everything"
           )

main :: IO ()
main = program =<< execParser opts
 where
  opts = info (options <**> helper)
    ( fullDesc
    <> progDesc "Radon REPL"
    <> header "radon - radon language compiler" )
