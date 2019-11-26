-----------------------------------------------------------------------------
-- |
-- Module      : Parser
-- Copyright   : Copyright (c) 2019 Ashley Towns
-- License     : BSD-style
-- Maintainer  : code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------
module Parser(parseFile) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec hiding (some, many)

import AST
import Parser.TopLevel (pTopLevel)

parseText :: Text -> Text -> [TL]
parseText name txt =
  case parse (pTopLevel <* eof) (T.unpack name) txt of
    Left err -> error $ errorBundlePretty err
    Right x -> x

parseFile :: FilePath -> IO [TL]
parseFile fp = do
  out <- T.readFile fp
  return $ parseText (T.pack fp) out
