{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, GADTs #-}
module Main where

import Options.Applicative
import System.Console.Haskeline

import Text.Pretty.Simple

import Parser
import Parser.TopLevel (pTopLevel)
import Parser.Statement (pStmt)

import TypeCheck

data Options = Options { verbose    :: Bool
                       } deriving (Show)

evalStatement :: Text -> Text
evalStatement inp =
  case parseText pStmt "<input>" inp of
    Left err -> "Syntax error: " <> err
    Right x  ->
      case typeCheckStatement x of
        Left err -> "Type error: " <> err
        Right x' -> toS $ pShow x'

evalTL :: Text -> Text
evalTL inp =
  case parseText pTopLevel "<input>" inp of
    Left _ -> evalStatement inp
    Right x  ->
      case traverse typeCheckTopLevel x of
        Left err -> "Type error: " <> err
        Right x' -> toS $ pShow x'

program :: Options -> IO ()
program Options{..} = do
  putStrLn "Radon REPL 0.000000001 \"Rounding Error\""
  runInputT defaultSettings loop
 where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "% "
    case minput of
      Nothing -> pure ()
      Just "quit" -> pure ()
      Just "" -> loop
      Just input -> do
        outputStrLn (toS $ evalTL $ toS input)
        -- outputStrLn (toS $ evalShowable [PC.showParser pTopLevel, PC.showParser pStmt] $ toS input)
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
