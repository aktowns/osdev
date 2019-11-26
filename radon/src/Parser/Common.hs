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

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST

newtype ParserError = ParserError Text deriving (Show, Eq, Ord)

instance ShowErrorComponent ParserError where
  showErrorComponent (ParserError msg) = toS msg
  errorComponentLen _ = 1

type Parser = Parsec ParserError Text

--------------------
-- symbols
----
-- | :
colon :: Parser Text
colon      = symbol ":"

-- | ::
coloncolon :: Parser Text
coloncolon = symbol "::"

-- | ;
semi :: Parser Text
semi       = symbol ";"

-- | .
dot :: Parser Text
dot        = symbol "."

-- | (
lparen :: Parser Text
lparen     = symbol "("

-- | )
rparen :: Parser Text
rparen     = symbol ")"

-- | <
langle :: Parser Text
langle     = symbol "<"

-- | >
rangle :: Parser Text
rangle     = symbol ">"

-- | {
lbrace :: Parser Text
lbrace     = symbol "{"

-- | }
rbrace :: Parser Text
rbrace     = symbol "}"

-- | [
lbrack :: Parser Text
lbrack     = symbol "["

-- | ]
rbrack :: Parser Text
rbrack     = symbol "]"

-- | {%
lbraceper :: Parser Text
lbraceper  = symbol "{%"

-- | %}
rbraceper :: Parser Text
rbraceper  = symbol "%}"

-- | <-
larrow :: Parser Text
larrow     = symbol "<-"

-- | ->
rarrow :: Parser Text
rarrow     = symbol "->"

-- | =
equals :: Parser Text
equals     = symbol "="

-- | ==
eqeq :: Parser Text
eqeq       = symbol "=="

-- | !=
neq :: Parser Text
neq        = symbol "!="

-- | <=
langleeq :: Parser Text
langleeq   = symbol "<="

-- | >=
rangleeq :: Parser Text
rangleeq   = symbol ">="

-- | ,
comma :: Parser Text
comma      = symbol ","

-- | +
plus :: Parser Text
plus       = symbol "+"

-- | ++
plusplus :: Parser Text
plusplus   = symbol "++"

-- | -
minus :: Parser Text
minus      = symbol "-"

-- | --
minusminus :: Parser Text
minusminus = symbol "--"

-- | *
star :: Parser Text
star       = symbol "*"

-- | /
fslash :: Parser Text
fslash     = symbol "/"

-- | \
bslash :: Parser Text
bslash     = symbol "\\"

-- | <<
shiftl :: Parser Text
shiftl     = symbol "<<"

-- | >>
shiftr :: Parser Text
shiftr     = symbol ">>"

-- | |
pipe :: Parser Text
pipe       = symbol "|"

-- | ||
pipepipe :: Parser Text
pipepipe   = symbol "||"

-- | &
amp :: Parser Text
amp        = symbol "&"

-- | &&
ampamp :: Parser Text
ampamp     = symbol "&&"

-- | ^
caret :: Parser Text
caret      = symbol "^"

-- | ...
varargs :: Parser Text
varargs    = symbol "..."

-- | static
kStatic :: Parser Text
kStatic  = symbol "static" <?> "static"

-- | inline
kInline :: Parser Text
kInline  = symbol "inline" <?> "inline"

-- | const
kConst :: Parser Text
kConst   = symbol "const" <?> "const"

-- | enum
kEnum :: Parser Text
kEnum    = symbol "enum" <?> "enum"

-- | struct
kStruct :: Parser Text
kStruct  = symbol "struct" <?> "struct"

-- | union
kUnion :: Parser Text
kUnion   = symbol "union" <?> "union"

-- | return
kReturn :: Parser Text
kReturn  = symbol "return"

-- | val
kVal :: Parser Text
kVal     = symbol "val" <?> "var"

-- | while
kWhile :: Parser Text
kWhile   = symbol "while"

-- | for
kFor :: Parser Text
kFor     = symbol "for"

-- | module
kModule :: Parser Text
kModule  = symbol "module" <?> "module"

-- | type
kType :: Parser Text
kType    = symbol "type" <?> "type"

-- | alias
kAlias :: Parser Text
kAlias   = symbol "alias"

-- | import
kImport :: Parser Text
kImport  = symbol "import"

-- | if
kIf :: Parser Text
kIf      = symbol "if"

-- | elif
kElif :: Parser Text
kElif    = symbol "elif"

-- | else
kElse :: Parser Text
kElse    = symbol "else"

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

stringLiteral :: Parser Text
stringLiteral = toS <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

identifier :: Parser Text
identifier =
  lexeme $
    toS <$> ((:) <$> lowerChar <*> many (alphaNumChar <|> char '_') <?> "identifier")

cIdentifier :: Parser Text
cIdentifier = lexeme $ toS <$> ((:) <$> upperChar <*> many alphaNumChar <?> "identifier")

language :: Parser Language
language = C <$ (char 'C' *> space)

parens :: Parser a -> Parser a
parens = between lparen rparen

braces :: Parser a -> Parser a
braces = between lbrace rbrace

angles :: Parser a -> Parser a
angles = between langle rangle

bracks :: Parser a -> Parser a
bracks = between lbrack rbrack

bracesper :: Parser a -> Parser a
bracesper = between lbraceper rbraceper

--------------------
-- utils
----
getNA :: Parser NodeAnnotation
getNA = do
  pos <- getSourcePos
  pure $ NodeAnnotation { source = NodeSource (toS $ sourceName pos) (unPos $ sourceLine pos) (unPos $ sourceColumn pos)
                        , metadata = NodeMetadata { codegenIgnore = False, rewriterIgnore = False }
                        }

