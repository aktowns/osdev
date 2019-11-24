module Analyzers.Analyzer where

import Data.Text (Text)

import AST

data AnalyzerResult = Ok | Failed Text deriving (Show)

class Analyzer a where
  analyze :: a -> [TL] -> IO AnalyzerResult
