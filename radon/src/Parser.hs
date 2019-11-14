module Parser(parseFile) where

import qualified Data.Text.IO as T
import Text.Megaparsec hiding (some, many)

import AST
import Parser.TopLevel (pTopLevel)

parseFile :: FilePath -> IO [TL]
parseFile fp = do 
  out <- T.readFile fp
  case parse (pTopLevel <* eof) fp out of
    Left err -> error $ errorBundlePretty err
    Right x -> return x
