{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.StatementSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.RawString.QQ
import Text.Megaparsec (parse)

import Parser.Statement
import AST

import Parser.Utils

spec :: Spec
spec = do
  describe "return" $ do
    it "parses return with no value" $ do
      let source = "return"
      parseAST pReturn source `shouldParse` Return Nothing ()

    it "parses return with an expression" $ do
      let source = "return 0"
      parseAST pReturn source `shouldParse` Return (Just (Literal (IntLiteral 0 Dec []) ())) ()

  describe "val" $ do
    it "declares a variable with no value" $ do
      let source = "val x: Y"
      parseAST pDeclare source `shouldParse` Return Nothing ()
    it "declares a variable with an expression" $ do
      let source = "val x: Y = 1"
      parseAST pDeclare source `shouldParse` Declare "x"
        (TyDef "Y") (Just (Literal (IntLiteral 1 Dec []) ())) ()
    it "declares a static inline const variable" $ do
      let source = "static inline const val x: Y = 1"
      parseAST pDeclare source `shouldParse` Declare "x"
        (TyStatic (TyInline (TyConst (TyDef "Y")))) (Just (Literal (IntLiteral 1 Dec []) ())) ()

  describe "while" $ do
    it "parses a single line while statement" $ do
      let source = "while (1): len++"
      parseAST pWhile source `shouldParse` While
        (Literal (IntLiteral 1 Dec []) ())
        [SExpr (Unary UnaryPostfix Increment (Identifier "len" ()) ()) ()] ()
    it "parses a multiline while statment" $ do
      let source = deindent [r|
        while (1):
          len++
      |]
      parseAST pWhile source `shouldParse` While
        (Literal (IntLiteral 1 Dec []) ())
        [SExpr (Unary UnaryPostfix Increment (Identifier "len" ()) ()) ()] ()

  describe "for" $ do
    it "parses a single line for statement" $ do
      let source = "for (val i: Size = 0; i < 10; i++): len++"
      parseAST pFor source `shouldParse` For
        (Declare "i" (TyDef "Size") (Just (Literal (IntLiteral 0 Dec []) ())) ())
        (Binary LessThan (Identifier "i" ()) (Literal (IntLiteral 10 Dec []) ()) ())
        (Unary UnaryPostfix Increment (Identifier "i" ()) ())
        [SExpr (Unary UnaryPostfix Increment (Identifier "len" ()) ()) ()] ()

    it "parses a multiline for statement" $ do
      let source = deindent [r|
        for (val i: Size = 0; i < 10; i++):
          len++
      |]
      parseAST pFor source `shouldParse` For
        (Declare "i" (TyDef "Size") (Just (Literal (IntLiteral 0 Dec []) ())) ())
        (Binary LessThan (Identifier "i" ()) (Literal (IntLiteral 10 Dec []) ()) ())
        (Unary UnaryPostfix Increment (Identifier "i" ()) ())
        [SExpr (Unary UnaryPostfix Increment (Identifier "len" ()) ()) ()] ()
