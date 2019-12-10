{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Analyzers.Analyzer
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- Typeclass for tree analyzers
-----------------------------------------------------------------------------
module Analyzers.Analyzer where

import AST

data AnalyzerResult = Ok | Failed Text deriving (Show)

class Analyzer a b where
  analyze :: a -> [TopLevel b] -> IO AnalyzerResult
