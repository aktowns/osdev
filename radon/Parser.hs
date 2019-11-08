{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Parser where

import Control.Applicative
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec hiding (some, many)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST

type Parser = Parsec Void Text

colon  = symbol ":"
lparen = symbol "("
rparen = symbol ")"
lt     = symbol "<"
gt     = symbol ">"
equals = symbol "="
comma  = symbol ","

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

identifier :: Parser Text
identifier = lexeme $ T.pack <$> ((:) <$> lowerChar <*> many alphaNumChar <?> "Identifier")

cIdentifier :: Parser Text
cIdentifier = lexeme $ T.pack <$> ((:) <$> upperChar <*> many alphaNumChar <?> "Identifier")

--pVariable :: Parser Expr
--pVariable = Var <$> lexeme
--  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Literal . IntLiteral <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

pUnaryType :: Parser Type
pUnaryType = cIdentifier <&> \ident ->
  case ident of
    "Void" -> TyVoid
    "Int"  -> TyInt
    "Char" -> TyChar
    "String" -> TyPtr TyChar

pWrapType :: Parser Type
pWrapType = do
  outer <- symbol "Ptr"
  _ <- lt
  inner <- pType
  _ <- gt
  return $ TyPtr inner 

pType = pWrapType <|> pUnaryType

pArgs :: Parser [(Text, Type)]
pArgs = ((,) <$> (identifier <* colon) <*> pType) `sepBy` comma

pFunc :: Parser TopLevel
pFunc = L.nonIndented scn (L.indentBlock scn preamb)
 where 
  preamb = do
    name <- identifier
    _ <- lparen
    args <- pArgs
    _ <- rparen
    _ <- colon
    typ <- pType
    _ <- equals
    return $ L.IndentSome Nothing (\x -> return $ Func name typ args $ S <$> x) pStmt

pReturn :: Parser Stmt
pReturn = Return <$> (symbol "return" *> optional pInteger)

pStmt = pReturn

parseFile :: FilePath -> IO TopLevel
parseFile fp = do 
  out <- T.readFile fp
  case parse (pFunc <* eof) fp out of
    Left err -> error $ errorBundlePretty err
    Right x -> return x
