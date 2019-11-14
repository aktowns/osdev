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
        Func "hello" TyVoid [] [SExpr (Literal (IntLiteral 1 Dec []) ()) ()] ()
    it "parses a function with an argument" $ do
      let source = deindent [r|
        hello(argc: Int): Void =
          1
      |]
      parseAST pFunc source `shouldParse`
        Func "hello" TyVoid [("argc",TyDef "Int")] [SExpr (Literal (IntLiteral 1 Dec []) ()) ()] ()

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
