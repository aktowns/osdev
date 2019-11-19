-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.Common
-- Copyright   :  Copyright (c) 2019 Ashley Towns
-- License     :  BSD-style
-- Maintainer  :  code@ashleytowns.id.au
-- Stability   : experimental
-- Portability : portable
--
-- Some parser utils/defs used throughout the parsers
-----------------------------------------------------------------------------
module Parser.Common where

import Control.Monad (void)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST

data ParserError = ParserError Text deriving (Show, Eq, Ord)

instance ShowErrorComponent ParserError where
  showErrorComponent (ParserError msg) = T.unpack $ msg
  errorComponentLen _ = 1

type Parser = Parsec ParserError Text

--------------------
-- symbols
----
colon :: Parser Text
colon      = symbol ":"

coloncolon :: Parser Text
coloncolon = symbol "::"

semi :: Parser Text
semi       = symbol ";"

dot :: Parser Text
dot        = symbol "."

lparen :: Parser Text
lparen     = symbol "("

rparen :: Parser Text
rparen     = symbol ")"

langle :: Parser Text
langle     = symbol "<"

rangle :: Parser Text
rangle     = symbol ">"

lbrace :: Parser Text
lbrace     = symbol "{"

rbrace :: Parser Text
rbrace     = symbol "}"

lbrack :: Parser Text
lbrack     = symbol "["

rbrack :: Parser Text
rbrack     = symbol "]"

larrow :: Parser Text
larrow     = symbol "<-"

rarrow :: Parser Text
rarrow     = symbol "->"

equals :: Parser Text
equals     = symbol "="

eqeq :: Parser Text
eqeq       = symbol "=="

neq :: Parser Text
neq        = symbol "!="

langleeq :: Parser Text
langleeq   = symbol "<="

rangleeq :: Parser Text
rangleeq   = symbol ">="

comma :: Parser Text
comma      = symbol ","

plus :: Parser Text
plus       = symbol "+"

plusplus :: Parser Text
plusplus   = symbol "++"

minus :: Parser Text
minus      = symbol "-"

minusminus :: Parser Text
minusminus = symbol "--"

star :: Parser Text
star       = symbol "*"

fslash :: Parser Text
fslash     = symbol "/"

bslash :: Parser Text
bslash     = symbol "\\"

shiftl :: Parser Text
shiftl     = symbol "<<"

shiftr :: Parser Text
shiftr     = symbol ">>"

pipe :: Parser Text
pipe       = symbol "|"

pipepipe :: Parser Text
pipepipe   = symbol "||"

amp :: Parser Text
amp        = symbol "&"

ampamp :: Parser Text
ampamp     = symbol "&&"

caret :: Parser Text
caret      = symbol "^"

kStatic :: Parser Text
kStatic  = symbol "static" <?> "static"

kInline :: Parser Text
kInline  = symbol "inline" <?> "inline"

kConst :: Parser Text
kConst   = symbol "const" <?> "const"

kFunc :: Parser Text
kFunc    = symbol "func"

kEnum :: Parser Text
kEnum    = symbol "enum" <?> "enum"

kReturn :: Parser Text
kReturn  = symbol "return"

kVal :: Parser Text
kVal     = symbol "val" <?> "var"

kWhile :: Parser Text
kWhile   = symbol "while"

kFor :: Parser Text
kFor     = symbol "for"

kModule :: Parser Text
kModule  = symbol "module" <?> "module"

kType :: Parser Text
kType    = symbol "type" <?> "type"

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

hexadecimal :: Parser Integer
hexadecimal = lexeme L.hexadecimal

octal :: Parser Integer
octal = lexeme L.octal

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

parens :: Parser a -> Parser a
parens = between lparen rparen

braces :: Parser a -> Parser a
braces = between lbrace rbrace

angles :: Parser a -> Parser a
angles = between langle rangle

bracks :: Parser a -> Parser a
bracks = between lbrack rbrack

--------------------
-- utils
----
getNA :: Parser NodeAnnotation
getNA = do
  pos <- getSourcePos
  return $ NodeAnnotation (sourceName pos)
                          (unPos $ sourceLine pos)
                          (unPos $ sourceColumn pos)

