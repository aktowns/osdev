{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Rewriters.C.FunctionAlias
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- Find and alias external C functions
-----------------------------------------------------------------------------
module Rewriters.C.FunctionAlias where

import Language.C
import Language.C.System.GCC

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

import System.IO (hClose)
import System.IO.Temp(withSystemTempFile)

import AST
import Rewriters.Rewriter

-- | Looks for external alias references and builds aliases them with an asm label
-- since this provides no type safety, it looks up and defines the reference for a function
-- itself at compile time, ie printf would resolve to (format: String, ...)
--
-- > alias prontf = printf
--
-- or optionally
--
-- > alias printf
--
-- which by itself doesn't make much sense but is usually used to hoist a C function
-- into radon under a module, something like
--
-- > module Console =
-- >   alias printf
-- >
-- > Console::printf("Hello World")
--
data FunctionAliases = FunctionAliases { cFuncs :: Map Text CDecl
                                       , aliases :: Map Text Text
                                       }

functionAliases :: FunctionAliases
functionAliases = FunctionAliases { cFuncs = Map.empty, aliases = Map.empty }

instance Extractor FunctionAliases [CExtDecl] where
  extract _ xs = do
    let headers = catMaybes $ collectImports <$> xs
    Right (CTranslUnit decls _) <- withSystemTempFile "resolver-function-alias.c" $ \fp hndl -> do
      T.hPutStrLn hndl $ T.unlines headers
      hClose hndl
      parseCFile (newGCC "gcc") Nothing [] fp
    pure decls

collectImports :: TL -> Maybe Text
collectImports (Import (Just C) text _) = Just $ "#include <" <> text <> ".h>"
collectImports _ = Nothing
