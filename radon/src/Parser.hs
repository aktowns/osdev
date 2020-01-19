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
module Parser(parseFile, parseText) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec hiding (some, many)

import AST.Phases.Parsed (ToplPA)

import Parser.Common (Parser)
import Parser.TopLevel (pTopLevel)

parseText :: Parser a -> Text -> Text -> Either Text a
parseText p name txt = first (toS . errorBundlePretty) parsed
 where parsed = parse (p <* eof) (T.unpack name) txt

parseAllText :: Text -> Text -> Either Text [ToplPA]
parseAllText = parseText pTopLevel

parseFile :: FilePath -> IO (Either Text [ToplPA])
parseFile fp = do
  out <- T.readFile fp
  pure $ parseAllText (T.pack fp) out
