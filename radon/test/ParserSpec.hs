{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.RawString.QQ
import Text.Megaparsec (parse)

import Parser
import AST

import Parser.Utils
import Parser.TopLevel

spec :: Spec
spec =
  describe "overall" $ do
    it "declares a variable with an expression" $ do
      let source = deindent [r|
        static const val VGAWidth: Size = 80
      |]
      parseASTList pTopLevel source `shouldParse` [Decl "VGAWidth" (TyStatic (TyConst (TyDef "Size")))
        (Just (Literal (IntLiteral 80 Dec []) ())) ()]

    it "declares a static inline const variable" $ do
      let source = "static inline const val X: Y = 1"
      parseASTList pTopLevel source `shouldParse` [Decl "X" (TyStatic (TyInline (TyConst (TyDef "Y"))))
        (Just (Literal (IntLiteral 1 Dec []) ())) ()]

    it "parses a function with multiple statements" $ do
      let source = deindent [r|
        hello: Void =
          val x: Int32 = 1
          x = 2
      |]
      parseASTList pTopLevel source `shouldParse`
        [Func "hello" TyVoid [] [
          Declare "x" (TyDef "Int32") (Just (Literal (IntLiteral 1 Dec []) ())) (),
          SExpr (Assign (Identifier "x" ()) (Literal (IntLiteral 2 Dec []) ()) ()) ()
        ] ()]

    it "parses multiple functions" $ do
      let source = deindent [r|
        hello: Void =
          x = 2
        world: Void =
          y = 3
      |]
      parseASTList pTopLevel source `shouldParse`
        [Func "hello" TyVoid [] [
            SExpr (Assign (Identifier "x" ()) (Literal (IntLiteral 2 Dec []) ()) ()) ()
          ] (),
         Func "world" TyVoid [] [
            SExpr (Assign (Identifier "y" ()) (Literal (IntLiteral 3 Dec []) ()) ()) ()
          ]
        ()]
