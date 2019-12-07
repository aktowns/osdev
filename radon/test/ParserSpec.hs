{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.RawString.QQ

import AST
import AST.Phases.Undecorated

import Parser.Utils
import Parser.TopLevel

spec :: Spec
spec =
  describe "overall" $ do
    it "declares a variable with an expression" $ do
      let source = deindent [r|
        static const val VGAWidth: Size = 80
      |]
      parseASTList pTopLevel source `shouldParse` [DeclUD "VGAWidth" (TyStatic (TyConst (TyDef "Size")))
        (Just (LiteralUD (IntLiteral 80 Dec [])))]

    it "declares a static inline const variable" $ do
      let source = "static inline const val X: Y = 1"
      parseASTList pTopLevel source `shouldParse` [DeclUD "X" (TyStatic (TyInline (TyConst (TyDef "Y"))))
        (Just (LiteralUD (IntLiteral 1 Dec [])))]

    it "parses a function with multiple statements" $ do
      let source = deindent [r|
        hello: Void =
          val x: Int32 = 1
          x = 2
      |]
      parseASTList pTopLevel source `shouldParse`
        [FuncUD "hello" TyVoid ([], False) [
          DeclareUD "x" (TyDef "Int32") (Just (LiteralUD (IntLiteral 1 Dec []))),
          SExprUD (AssignUD (IdentifierUD "x") (LiteralUD (IntLiteral 2 Dec [])))
         ]
        ]

    it "parses multiple functions" $ do
      let source = deindent [r|
        hello: Void =
          x = 2
        world: Void =
          y = 3
      |]
      parseASTList pTopLevel source `shouldParse`
        [FuncUD "hello" TyVoid ([], False) [
            SExprUD (AssignUD (IdentifierUD "x") (LiteralUD (IntLiteral 2 Dec [])))
         ],
         FuncUD "world" TyVoid ([], False) [
            SExprUD (AssignUD (IdentifierUD "y") (LiteralUD (IntLiteral 3 Dec [])))
         ]
        ]
