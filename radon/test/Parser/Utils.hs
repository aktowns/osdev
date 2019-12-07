module Parser.Utils where

import Text.Megaparsec (parse)
import qualified Data.Text as T

import AST.Phases.Undecorated
import AST.Phases.Parsed
import Data.Functor ((<&>), void)

parseAST p s  = parse p "" s <&> toUndecorated
parseASTList p s = fmap toUndecorated <$> parse p "" s
parseAST' p = parse p ""
deindent s = T.unlines $ map (T.drop 8) $ T.lines s
