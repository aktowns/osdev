-- | An opinionated prelude mixin
module Prelude ( module BasePrelude
               , module Exports
               , tail, head
               , concatMapM
               , error
               , undefined) where

import BasePrelude hiding ( String                        -- use text
                          , head, tail, last, init, read  -- use total alts
                          , map                           -- use fmap
                          , return                        -- use pure
                          , error                         -- added warning
                          , undefined                     -- added warning
                          )

import qualified BasePrelude as P

import Data.Functor as Exports ((<&>))
import Data.Text as Exports (Text)    -- Bring Text into scope by default
import Data.Map as Exports (Map)      -- Bring Map into scope by default
import Data.Set as Exports (Set)

import Data.Int as Exports
import Data.Tuple as Exports
import Data.Maybe as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports
import Data.Bifunctor as Exports

import Data.String.Conv as Exports (toS)

import Control.Monad as Exports ( join
                                , guard
                                , when
                                , unless
                                )

head :: [a] -> Maybe a
{-# INLINE head #-}
head = listToMaybe

tail :: [a] -> [a]
{-# INLINE tail #-}
tail = drop 1

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM f xs = concat <$> mapM f xs

undefined :: a
{-# WARNING undefined "'undefined' remains in code" #-}
undefined = P.undefined

error :: P.String -> a
{-# WARNING error "'error' remains in code" #-}
error = P.error
