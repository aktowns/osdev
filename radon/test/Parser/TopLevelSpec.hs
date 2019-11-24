{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.TopLevelSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.RawString.QQ
import Text.Megaparsec (parse)

import Parser.TopLevel
import AST

import Parser.Utils

spec :: Spec
spec = do
  describe "func" $ do
    it "parses a function with no arguments" $ do
      let source = deindent [r|
        hello: Void =
          1
      |]
      parseAST pFunc source `shouldParse`
        Func "hello" TyVoid ([], False) [SExpr (Literal (IntLiteral 1 Dec []) ()) ()] ()
    it "parses a function with an argument" $ do
      let source = deindent [r|
        hello(argc: Int): Void =
          1
      |]
      parseAST pFunc source `shouldParse`
        Func "hello" TyVoid ([("argc",TyDef "Int")], False) [SExpr (Literal (IntLiteral 1 Dec []) ()) ()] ()
    it "parses a function with varargs" $ do
      let source = deindent [r|
        hello(argc: Int, ...): Void =
          1
      |]
      parseAST pFunc source `shouldParse`
        Func "hello" TyVoid ([("argc",TyDef "Int")], True) [SExpr (Literal (IntLiteral 1 Dec []) ()) ()] ()
    it "parses a function with multiple statements" $ do
      let source = deindent [r|
        hello: Void =
          val x: Int32 = 1
          x = 2
      |]
      parseAST pFunc source `shouldParse`
        Func "hello" TyVoid ([], False) [
          Declare "x" (TyDef "Int32") (Just (Literal (IntLiteral 1 Dec []) ())) (),
          SExpr (Assign (Identifier "x" ()) (Literal (IntLiteral 2 Dec []) ()) ()) ()
        ] ()

  describe "enum" $ do
    it "parses a simple enum" $ do
      let source = deindent [r|
        enum MyEnum =
          Black = 0
          Blue  = 1
      |]
      parseAST pEnum source `shouldParse`
        Enum "MyEnum" [("Black", Just 0), ("Blue", Just 1)] ()
    it "doesn't require values" $ do
      let source = deindent [r|
        enum MyEnum =
          Black
          Blue
      |]
      parseAST pEnum source `shouldParse`
        Enum "MyEnum" [("Black", Nothing), ("Blue", Nothing)] ()

  describe "val" $ do
    it "declares a variable with no value" $ do
      let source = "val X: Y"
      parseAST pDecl source `shouldParse` Decl "X" (TyDef "Y") Nothing ()
    it "declares a variable with an expression" $ do
      let source = "val X: Y = 1"
      parseAST pDecl source `shouldParse` Decl "X"
        (TyDef "Y") (Just (Literal (IntLiteral 1 Dec []) ())) ()
    it "declares a static inline const variable" $ do
      let source = "static inline const val X: Y = 1"
      parseAST pDecl source `shouldParse` Decl "X"
        (TyStatic (TyInline (TyConst (TyDef "Y")))) (Just (Literal (IntLiteral 1 Dec []) ())) ()

  describe "modules" $ do
    it "parses a simple module" $ do
      let source = deindent [r|
        module ABC =
          val ItsEasy: Int32 = 0
          val As123: Int32   = 1
      |]
      parseAST pModule source `shouldParse` Module "ABC" [
        Decl "ItsEasy" (TyDef "Int32") (Just (Literal (IntLiteral 0 Dec []) ())) (),
        Decl "As123" (TyDef "Int32") (Just (Literal (IntLiteral 1 Dec []) ())) ()
       ] ()

    it "parses a more advanced module" $ do
      let source = deindent [r|
        module MyModule =
          val SomeGlobal: Int32 = 0

          initialize: Void =
            SomeGlobal = 1
      |]
      parseAST pModule source `shouldParse` Module "MyModule" [
        Decl "SomeGlobal" (TyDef "Int32") (Just (Literal (IntLiteral 0 Dec []) ())) (),
        Func "initialize" TyVoid ([],False) [SExpr (Assign (Identifier "SomeGlobal" ()) (Literal (IntLiteral 1 Dec []) ()) ()) ()] ()
       ] ()
