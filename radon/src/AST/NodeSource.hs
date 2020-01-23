{-# LANGUAGE RecordWildCards #-}
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

import AST.Base

data NodeSource = NodeSource { filename :: Text
                             , line     :: Int
                             , column   :: Int
                             }

instance Show NodeSource where
  show NodeSource{..} = (toS filename) <> ":" <> (show line) <> ":" <> (show column)

defaultNodeSource :: NodeSource
defaultNodeSource = NodeSource { filename = "<no file>"
                               , line = 0
                               , column = 0
                               }

data TypedSource = TypedSource { nodeSource :: NodeSource
                               , typed      :: Type
                               }

instance Show TypedSource where
  show TypedSource{..} = show nodeSource <> " [" <> show typed <> "]"
