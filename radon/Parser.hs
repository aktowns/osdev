{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase #-}
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
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

import AST

import Language.C.Data.Position (position)
import Language.C.Data.Node (NodeInfo, mkNodeInfoOnlyPos)

type Parser = Parsec Void Text


getNI :: Parser NodeInfo
getNI = do
  pos <- getSourcePos
  return $ mkNodeInfoOnlyPos (position 0 (sourceName pos) (unPos $ sourceLine pos) (unPos $ sourceColumn pos) Nothing)

colon    = symbol ":"
semi     = symbol ";"
dot      = symbol "."
lparen   = symbol "("
rparen   = symbol ")"
langle   = symbol "<"
rangle   = symbol ">"
lbrace   = symbol "{"
rbrace   = symbol "}"
lbrack   = symbol "["
rbrack   = symbol "]"
larrow   = symbol "<-"
rarrow   = symbol "->"
equals   = symbol "="
comma    = symbol ","
plus     = symbol "+"
plusplus = symbol "++"
minus    = symbol "-"
star     = symbol "*"
fslash   = symbol "/"
bslash   = symbol "\\"
shiftl   = symbol "<<"
shiftr   = symbol ">>"
pipe     = symbol "|"
pipeipe  = symbol "||"
and      = symbol "&"
andand   = symbol "&&"
 
kStatic  = symbol "static"
kInline  = symbol "inline"
kFunc    = symbol "func"
kEnum    = symbol "enum"
kReturn  = symbol "return"

table = [ [ prefix  minus    $ Unary  UnaryPrefix  Negate
          , prefix  plus     $ Unary  UnaryPrefix  Positive ]
        , [ postfix plusplus $ Unary  UnaryPostfix Increment ]
        , [ binary  star     $ Binary Mul
          , binary  fslash   $ Binary Div ]
        , [ binary  plus     $ Binary Add 
          , binary  minus    $ Binary Sub ] ]

binary  name f = InfixL  $ f <$ name
prefix  name f = Prefix  $ f <$ name
postfix name f = Postfix $ f <$ name

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

stringLiteral' :: Parser Text
stringLiteral' = T.pack <$> stringLiteral

identifier :: Parser Text
identifier = 
  lexeme $ 
    T.pack <$> ((:) <$> lowerChar <*> many (alphaNumChar <|> char '_') <?> "identifier")

cIdentifier :: Parser Text
cIdentifier = lexeme $ T.pack <$> ((:) <$> upperChar <*> many alphaNumChar <?> "identifier")

int :: Parser Int
int = lexeme L.decimal

pInteger :: Parser (Expr NodeInfo)
pInteger = Literal . IntLiteral <$> lexeme L.decimal <*> getNI

pString :: Parser (Expr NodeInfo)
pString = Literal . StrLiteral <$> stringLiteral' <*> getNI

parens :: Parser a -> Parser a
parens = between lparen rparen

braces :: Parser a -> Parser a
braces = between lbrace rbrace

angles :: Parser a -> Parser a
angles = between langle rangle

bracks :: Parser a -> Parser a
bracks = between lbrack rbrack

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

pUnaryType :: Parser Type
pUnaryType = cIdentifier <&> \case
  "Void"   -> TyVoid
  "Int"    -> TyInt
  "Char"   -> TyChar
  "String" -> TyPtr TyChar
  n        -> TyDef n

pWrapType :: Parser Type
pWrapType = do
  outer <- symbol "Ptr"
  inner <- angles pType
  return $ TyPtr inner 

pType :: Parser Type
pType = pWrapType <|> pUnaryType

pArgs :: Parser [(Text, Type)]
pArgs = ((,) <$> (identifier <* colon) <*> pType) `sepBy` comma

pEnumBody :: Parser (Text, Maybe Integer)
pEnumBody = (,) <$> cIdentifier <*> optional (equals *> integer)

pEnum :: Parser (TopLevel NodeInfo)
pEnum = L.nonIndented scn (L.indentBlock scn preamb)
 where
  preamb = do
    pos <- getNI
    _ <- kEnum
    name <- cIdentifier
    _ <- equals
    return $ L.IndentSome Nothing (\x -> return $ Enum name x pos) pEnumBody

pFuncPreamb :: Parser (NodeInfo, [Text], Text, [(Text, Type)], Type)
pFuncPreamb = do
    pos <- getNI
    quals <- many (kStatic <|> kInline)
    name <- identifier
    args <- parens pArgs
    _ <- colon
    typ <- pType
    _ <- equals
    return (pos, quals, name, args, typ)

pFuncSmall :: Parser (TopLevel NodeInfo)
pFuncSmall = L.nonIndented scn preamb
 where 
  preamb = do
    (pos, quals, name, args, typ) <- pFuncPreamb
    body <- pFunBody
    return $ Func name typ args [body] pos

pFuncFull :: Parser (TopLevel NodeInfo)
pFuncFull = L.nonIndented scn (L.indentBlock scn preamb)
 where 
  preamb = do
    (pos, quals, name, args, typ) <- pFuncPreamb
    return $ L.IndentSome Nothing (\x -> return $ Func name typ args x pos) pFunBody

pFunc :: Parser (TopLevel NodeInfo)
pFunc = pFuncFull -- <|> pFuncSmall)

pFuncCall :: Parser (Expr NodeInfo)
pFuncCall = do
  pos <- getNI
  name <- identifier
  args <- parens $ pExpr `sepBy` comma
  return $ FunCall name args pos

pExpr :: Parser (Expr NodeInfo)
pExpr = pString 
    <|> pInteger 
    <|> pBin 
    <|> pFuncCall 
    <|> pIdentifier

pIdentifier :: Parser (Expr NodeInfo)
pIdentifier = Identifier <$> identifier <*> getNI

pBinOp :: Parser BinaryOp
pBinOp = (Add <$ plus) 
     <|> (Sub <$ minus) 
     <|> (Mul <$ star) 
     <|> (Div <$ fslash)
     <|> (BitwiseOr <$ pipe)
     <|> (ShiftLeft <$ shiftl)

pBin :: Parser (Expr NodeInfo)
pBin = do
  pos <- getNI
  e1 <- pExpr
  o  <- pBinOp
  e2 <- pExpr
  return $ Binary o e1 e2 pos

pReturn :: Parser (Stmt NodeInfo)
pReturn = Return <$> (kReturn *> optional pInteger) <*> getNI

pStmt :: Parser (Stmt NodeInfo)
pStmt = pReturn

pFunBody :: Parser (Node NodeInfo)
pFunBody = (S <$> pStmt) <|> (E <$> pExpr)

pTopLevel :: Parser [TopLevel NodeInfo]
pTopLevel = many (pEnum <|> pFunc)

parseFile :: FilePath -> IO [TopLevel NodeInfo]
parseFile fp = do 
  out <- T.readFile fp
  case parse (pTopLevel <* eof) fp out of
    Left err -> error $ errorBundlePretty err
    Right x -> return x
