{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.ExpressionSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.RawString.QQ

import Parser.Utils

import Parser.Expression
import AST

spec :: Spec
spec = do
  describe "integer literals" $ do
    it "parses a simple integer" $ do
      let source = "1"
      parseAST pInteger source `shouldParse` Literal (IntLiteral 1 Dec []) ()
    it "parses a unsigned integer" $ do
      let source = "1u"
      parseAST pInteger source `shouldParse` Literal
        (IntLiteral 1 Dec [Unsigned]) ()
    it "parses a long integer" $ do
      let source = "1l"
      parseAST pInteger source `shouldParse` Literal
        (IntLiteral 1 Dec [Long]) ()
    it "parses a long long integer" $ do
      let source = "1ll"
      parseAST pInteger source `shouldParse` Literal
        (IntLiteral 1 Dec [LongLong]) ()
    it "parses a unsigned long long integer" $ do
      let source = "1llu"
      parseAST pInteger source `shouldParse` Literal
        (IntLiteral 1 Dec [LongLong, Unsigned]) ()

  describe "operators" $ do
    it "parses a negative integer" $ do
      let source = "-1"
      parseAST pExpr source `shouldParse` Unary UnaryPrefix Negate
        (Literal (IntLiteral 1 Dec []) ()) ()
    it "parses a positive integer" $ do
      let source = "+1"
      parseAST pExpr source `shouldParse` Unary UnaryPrefix Positive
        (Literal (IntLiteral 1 Dec []) ()) ()
    it "parses a prefix increment expression" $ do
      let source = "++1"
      parseAST pExpr source `shouldParse` Unary UnaryPrefix Increment
        (Literal (IntLiteral 1 Dec []) ()) ()
    it "parses a prefix decrement expression" $ do
      let source = "--1"
      parseAST pExpr source `shouldParse` Unary UnaryPrefix Decrement
        (Literal (IntLiteral 1 Dec []) ()) ()
    it "parses a postfix increment expression" $ do
      let source = "1++"
      parseAST pExpr source `shouldParse` Unary UnaryPostfix Increment
        (Literal (IntLiteral 1 Dec []) ()) ()
    it "parses a postfix decrement expression" $ do
      let source = "1--"
      parseAST pExpr source `shouldParse` Unary UnaryPostfix Decrement
        (Literal (IntLiteral 1 Dec []) ()) ()

  describe "array-sub" $ do
    it "parses a simple array subscript" $ do
      let source = "var[1]"
      parseAST pArraySub source `shouldParse`
        ArraySub "var" (Literal (IntLiteral 1 Dec []) ()) ()
    it "parses an array subscript with a function" $ do
      let source = "var[blah()]"
      parseAST pArraySub source `shouldParse`
        ArraySub "var" (FunCall "blah" [] ()) ()

  describe "func-call" $ do
    it "parses a function call with no arguments" $ do
      let source = "hello()"
      parseAST pFuncCall source `shouldParse` FunCall "hello" [] ()
    it "parses a function call with 1 expr" $ do
      let source = "hello(1)"
      parseAST pFuncCall source `shouldParse`
        FunCall "hello" [Literal (IntLiteral 1 Dec []) ()] ()
    it "parses multiple arguments" $ do
      let source = "hello(1,2,3)"
      parseAST pFuncCall source `shouldParse`
        FunCall "hello" [ Literal (IntLiteral 1 Dec []) ()
                        , Literal (IntLiteral 2 Dec []) ()
                        , Literal (IntLiteral 3 Dec []) ()] ()
    it "parses nested functions" $ do
      let source = "hello(world(1))"
      parseAST pFuncCall source `shouldParse`
        FunCall "hello" [ FunCall "world" [Literal (IntLiteral 1 Dec []) () ] ()] ()

  describe "assignment" $ do
    it "parses a local identifier assignment" $ do
      let source = "terminal = 1"
      parseAST pAssign source `shouldParse`
        Assign (Identifier "terminal" ()) (Literal (IntLiteral 1 Dec []) ()) ()
    it "parses a global identifier assignment" $ do
      let source = "Terminal = 1"
      parseAST pAssign source `shouldParse`
        Assign (Identifier "Terminal" ()) (Literal (IntLiteral 1 Dec []) ()) ()
