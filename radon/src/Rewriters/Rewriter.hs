module Rewriters.Rewriter where

import AST

class Rewriter a where
  rewrite :: a -> [TL] -> IO [TL]
