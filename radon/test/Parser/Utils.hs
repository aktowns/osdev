module Parser.Utils where

import Text.Megaparsec (parse)
import qualified Data.Text as T

import Data.Functor ((<&>), void)

parseAST p s  = parse p "" s <&> void
parseASTList p s = fmap void <$> parse p "" s
parseAST' p = parse p ""
deindent s = T.unlines $ map (T.drop 8) $ T.lines s
