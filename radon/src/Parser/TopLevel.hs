module Parser.TopLevel where

import Data.Text (Text)
import Control.Applicative hiding (some, many)
import Data.Maybe (fromMaybe)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Parser.Common
import Parser.Expression
import Parser.Type
import Parser.Statement

pEnum :: Parser TL
pEnum = L.nonIndented scn (L.indentBlock scn preamb) <?> "enum"
 where
  preamb = do
    pos <- getNA
    _ <- kEnum
    name <- cIdentifier
    _ <- equals
    return $ L.IndentSome Nothing (\x -> return $ Enum name x pos) pEnumBody

  pEnumBody :: Parser (Text, Maybe Integer)
  pEnumBody = (,) <$> cIdentifier <*> optional (equals *> integer)

pArgs :: Parser [(Text, Type)]
pArgs = ((,) <$> (identifier <* colon) <*> pType) `sepBy` comma

pFuncPreamb :: Parser (NodeAnnotation, [Text], Text, [(Text, Type)], Type)
pFuncPreamb = do
    pos <- getNA
    quals <- many (kStatic <|> kInline)
    name <- identifier
    args <- optional $ parens pArgs
    _ <- colon
    typ <- pType
    _ <- equals
    return (pos, quals, name, fromMaybe [] args, typ)

pFuncSmall :: Parser TL
pFuncSmall = L.nonIndented scn preamb
 where 
  preamb = do
    (pos, quals, name, args, typ) <- pFuncPreamb
    body <- pStmt
    return $ Func name typ args [body] pos

pFuncFull :: Parser TL
pFuncFull = L.indentBlock scn preamb
 where 
  preamb = do
    (pos, quals, name, args, typ) <- pFuncPreamb
    return $ L.IndentSome Nothing (\x -> return $ Func name typ args x pos) pStmt

pFunc :: Parser TL
pFunc = try pFuncSmall <|> pFuncFull

pDecl :: Parser TL
pDecl = do
  pos <- getNA
  quals <- many (kStatic <|> kConst)
  name <- kVal *> cIdentifier <* colon
  typ <- pType
  value <- optional $ equals *> pExpr <* eol
  return (Decl name typ value pos) <?> "val"

pModule :: Parser TL
pModule = L.nonIndented scn (L.indentBlock scn preamb)
 where 
  preamb = do
    pos <- getNA
    name <- kModule *> cIdentifier <* equals
    return $ L.IndentSome Nothing (\x -> return $ Module name x pos) pTopLevelEntry

pTopLevelEntry :: Parser TL
pTopLevelEntry = try pEnum <|> try pFunc <|> try pModule <|> pDecl

pTopLevel :: Parser [TL]
pTopLevel = many pTopLevelEntry