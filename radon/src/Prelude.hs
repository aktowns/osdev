-- | An opinionated prelude mixin
module Prelude ( module BasePrelude
               , (<&>)
               , Text
               , module Exports
               , tail, head) where

import BasePrelude hiding ( String            -- use text
                          , head, tail, read  -- use total alts
                          , map               -- use fmap
                          )

import Data.Functor((<&>)) -- Y U NOT STANDARD ANYWAY?
import Data.Text (Text)    -- Bring text into scope by default

import Data.Int as Exports
import Data.Tuple as Exports
import Data.Maybe as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports
import Data.Bifunctor as Exports

{-# INLINE head #-}
head :: [a] -> Maybe a
head = listToMaybe

{-# INLINE tail #-}
tail :: [a] -> [a]
tail = drop 1
