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
import Language.C.Syntax
import Language.C.System.GCC

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes)
import Data.Map (Map)
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

instance Rewriter FunctionAliases where
  rewrite fa xs = do
    headers <- catMaybes <$> mapM collectImports xs
    Right tree <- withSystemTempFile "resolver-function-alias.c" $ \fp hndl -> do
      T.hPutStrLn hndl $ T.unlines headers
      hClose hndl
      parseCFile (newGCC "gcc") Nothing [] fp
    print $ collectFunctions $ () <$ tree
    return xs

collectImports :: TL -> IO (Maybe Text)
collectImports (Import (Just C) text) = return . Just $ "#include <" <> text <> ".h>"
collectImports x = return Nothing

--isExtern :: [CDeclSpec] -> Bool
isExtern xs = any extern xs
 where
  --extern :: CDeclSpec -> Bool
  extern (CStorageSpec (CExtern _)) = True
  extern _ = False

isFun xs = any fun xs
 where
  fun :: (Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a)) -> Bool
  fun (Just (CDeclr _ xs _ _ _), _, _) = any fundeclr xs
  fun _ = False
  fundeclr (CFunDeclr _ _ _) = True
  fundeclr _ = False

--collectFunctions :: CTranslUnit -> [CDecl]
collectFunctions (CTranslUnit xs _) = catMaybes $ map collectDecls xs
 where
  --collectDecls :: CExtDecl -> Maybe CDecl
  collectDecls (CDeclExt x@(CDecl sp dec _)) =
    if isExtern sp && isFun dec then Just x else Nothing
  collectDecls _ = Nothing
