{-# LANGUAGE MultiParamTypeClasses #-}
module Rewriters.Rewriter where

import AST

class Rewriter a b where
  rewrite :: a -> [TopLevel b] -> IO [TopLevel b]

class Extractor a b c where
  extract :: a -> [TopLevel c] -> IO b
