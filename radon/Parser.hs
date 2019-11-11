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
eqeq     = symbol "=="
neq      = symbol "!="
langleeq = symbol "<="
rangleeq = symbol ">="
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
pipepipe = symbol "||"
amp      = symbol "&"
ampamp   = symbol "&&"
caret    = symbol "^"
 
kStatic  = symbol "static"
kInline  = symbol "inline"
kConst   = symbol "const"
kFunc    = symbol "func"
kEnum    = symbol "enum"
kReturn  = symbol "return"
kVal     = symbol "val"
kWhile   = symbol "while"
kModule  = symbol "module"

table = [ [ prefix  minus    $ Unary  UnaryPrefix  Negate
          , prefix  plus     $ Unary  UnaryPrefix  Positive ]
        , [ postfix plusplus $ Unary  UnaryPostfix Increment ]
        , [ binary  star     $ Binary Mul
          , binary  fslash   $ Binary Div ]
        , [ binary  plus     $ Binary Add  
          , binary  minus    $ Binary Sub ]
        , [ binary  shiftl   $ Binary ShiftLeft
          , binary  shiftr   $ Binary ShiftRight ]
        , [ binary  langle   $ Binary LessThan 
          , binary  langleeq $ Binary LessThanEqual
          , binary  rangle   $ Binary GreaterThan
          , binary  rangleeq $ Binary GreaterThanEqual ]
        , [ binary  eqeq     $ Binary Equals 
          , binary  neq      $ Binary NotEquals ]
        , [ binary  amp      $ Binary BitwiseAnd ]
        , [ binary  caret    $ Binary BitwiseXor ]
        , [ binary  pipe     $ Binary BitwiseOr ]
        , [ binary  ampamp   $ Binary LogicalAnd ]
        , [ binary  pipepipe $ Binary LogicalOr ]
        ]

binary :: Parser b -> (a -> a -> NodeInfo -> a) -> Operator Parser a
binary  name f = InfixL  $ (getNI <&> niflip3 f) <* name
 where
  niflip3 :: (a -> b -> c -> d) -> c -> a -> b -> d
  niflip3 f e3 e1 e2 = f e1 e2 e3

prefix :: Parser b -> (a -> NodeInfo -> a) -> Operator Parser a
prefix  name f = Prefix  $ (getNI <&> flip f) <* name

postfix :: Parser b -> (a -> NodeInfo -> a) -> Operator Parser a
postfix name f = Postfix $ (getNI <&> flip f) <* name

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
pEnum = L.nonIndented scn (L.indentBlock scn preamb) <?> "enum"
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
pFunc = try pFuncSmall <|> pFuncFull

pFuncCall :: Parser (Expr NodeInfo)
pFuncCall = do
  pos <- getNI
  name <- identifier
  args <- parens $ pExpr `sepBy` comma
  return $ FunCall name args pos

pTerm :: Parser (Expr NodeInfo)
pTerm = pString 
    <|> pInteger 
    <|> try pFuncCall 
    <|> try pArraySub
    <|> pIdentifier

pExpr = makeExprParser pTerm table

pIdentifier :: Parser (Expr NodeInfo)
pIdentifier = Identifier <$> identifier <*> getNI

pArraySub :: Parser (Expr NodeInfo)
pArraySub = do
  pos <- getNI
  ident <- identifier
  subsc <- bracks pExpr
  return $ ArraySub ident subsc pos

pReturn :: Parser (Stmt NodeInfo)
pReturn = Return <$> (kReturn *> optional pExpr) <*> getNI <?> "return"

pDeclare :: Parser (Stmt NodeInfo)
pDeclare = do
  pos <- getNI
  quals <- many (kStatic <|> kInline)
  _ <- kVal
  name <- identifier
  _ <- colon
  typ <- pType
  _ <- equals
  value <- optional pExpr
  return (Declare name typ value pos) <?> "val"

pWhileSmall :: Parser (Stmt NodeInfo)
pWhileSmall = do
  pos <- getNI
  _ <- kWhile
  cond <- parens pExpr
  _ <- colon
  body <- pStmt
  return $ While cond [body] pos

pWhile = pWhileSmall <?> "while"

pStmtExpr :: Parser (Stmt NodeInfo)
pStmtExpr = SExpr <$> pExpr <*> getNI

pStmt :: Parser (Stmt NodeInfo)
pStmt = pReturn <|> pDeclare <|> pWhile <|> pStmtExpr

pFunBody :: Parser (Node NodeInfo)
pFunBody = (S <$> pStmt) <|> (E <$> pExpr)

pDecl :: Parser (TopLevel NodeInfo)
pDecl = do
  pos <- getNI
  quals <- many (kStatic <|> kConst)
  name <- kVal *> cIdentifier <* colon
  typ <- pType
  value <- optional $ equals *> pExpr <* eol
  return (Decl name typ value pos) <?> "val"

pModule :: Parser (TopLevel NodeInfo)
pModule = dbg "module" $ L.nonIndented scn (L.indentBlock scn preamb)
 where 
  preamb = do
    pos <- getNI
    name <- kModule *> cIdentifier <* equals
    return $ L.IndentSome Nothing (\x -> return $ Module name x pos) pTopLevelEntry

pTopLevelEntry :: Parser (TopLevel NodeInfo)
pTopLevelEntry = try pEnum <|> try pFunc <|> try pModule <|> pDecl

pTopLevel :: Parser [TopLevel NodeInfo]
pTopLevel = many pTopLevelEntry

parseFile :: FilePath -> IO [TopLevel NodeInfo]
parseFile fp = do 
  out <- T.readFile fp
  case parse (pTopLevel <* eof) fp out of
    Left err -> error $ errorBundlePretty err
    Right x -> return x
