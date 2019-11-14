module Parser.Common where

import Data.Void
import Control.Monad (void)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.C.Data.Position (position)
import Language.C.Data.Node (NodeInfo, mkNodeInfoOnlyPos)

import AST

type Parser = Parsec Void Text

--------------------
-- symbols
----
colon      = symbol ":"
semi       = symbol ";"
dot        = symbol "."
lparen     = symbol "("
rparen     = symbol ")"
langle     = symbol "<"
rangle     = symbol ">"
lbrace     = symbol "{"
rbrace     = symbol "}"
lbrack     = symbol "["
rbrack     = symbol "]"
larrow     = symbol "<-"
rarrow     = symbol "->"
equals     = symbol "="
eqeq       = symbol "=="
neq        = symbol "!="
langleeq   = symbol "<="
rangleeq   = symbol ">="
comma      = symbol ","
plus       = symbol "+"
plusplus   = symbol "++"
minus      = symbol "-"
minusminus = symbol "--"
star       = symbol "*"
fslash     = symbol "/"
bslash     = symbol "\\"
shiftl     = symbol "<<"
shiftr     = symbol ">>"
pipe       = symbol "|"
pipepipe   = symbol "||"
amp        = symbol "&"
ampamp     = symbol "&&"
caret      = symbol "^"

kStatic  = symbol "static"
kInline  = symbol "inline"
kConst   = symbol "const"
kFunc    = symbol "func"
kEnum    = symbol "enum"
kReturn  = symbol "return"
kVal     = symbol "val"
kWhile   = symbol "while"
kFor     = symbol "for"
kModule  = symbol "module"

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

