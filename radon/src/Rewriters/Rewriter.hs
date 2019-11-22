{-# LANGUAGE MultiParamTypeClasses #-}
module Rewriters.Rewriter where

import AST

class Rewriter a where
  rewrite :: a -> [TL] -> IO [TL]

class Extractor a b where
  extract :: a -> [TL] -> IO b