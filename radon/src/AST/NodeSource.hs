-----------------------------------------------------------------------------
-- |
-- Module      : AST.NodeSource
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- The radon syntax tree - file/position info
-----------------------------------------------------------------------------
module AST.NodeSource where

data NodeSource = NodeSource { filename :: Text
                             , line     :: Int
                             , column   :: Int
                             } deriving (Show)

defaultNodeSource :: NodeSource
defaultNodeSource = NodeSource { filename = "<no file>"
                               , line = 0
                               , column = 0
                               }

