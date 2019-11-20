module Resolvers.Resolver where

import AST

class Resolver a where
  resolve :: a -> [TL] -> IO [TL]
