{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.StatementSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.RawString.QQ

import Parser.Statement
import AST
import AST.Phases.Undecorated

import Parser.Utils

spec :: Spec
spec = do
  describe "return" $ do
    it "parses return with no value" $ do
      let source = "return"
      parseAST pReturn source `shouldParse` ReturnUD Nothing

    it "parses return with an expression" $ do
      let source = "return 0"
      parseAST pReturn source `shouldParse` ReturnUD (Just (LiteralUD (IntLiteral 0 Dec [])))

  describe "val" $ do
    it "declares a variable with no value" $ do
      let source = "val x: Y"
      parseAST pDeclare source `shouldParse` DeclareUD "x" (TyDef "Y") Nothing
    it "declares a variable with an expression" $ do
      let source = "val x: Y = 1"
      parseAST pDeclare source `shouldParse` DeclareUD "x"
        (TyDef "Y") (Just (LiteralUD (IntLiteral 1 Dec [])))
    it "declares a static inline const variable" $ do
      let source = "static inline const val x: Y = 1"
      parseAST pDeclare source `shouldParse` DeclareUD "x"
        (TyStatic (TyInline (TyConst (TyDef "Y")))) (Just (LiteralUD (IntLiteral 1 Dec [])))

  describe "while" $ do
    it "parses a single line while statement" $ do
      let source = "while (1): len++"
      parseAST pWhile source `shouldParse` WhileUD
        (LiteralUD (IntLiteral 1 Dec []))
        [SExprUD (UnaryUD UnaryPostfix Increment (IdentifierUD "len"))]
    it "parses a multiline while statment" $ do
      let source = deindent [r|
        while (1):
          len++
      |]
      parseAST pWhile source `shouldParse` WhileUD
        (LiteralUD (IntLiteral 1 Dec []))
        [SExprUD (UnaryUD UnaryPostfix Increment (IdentifierUD "len"))]

  describe "for" $ do
    it "parses a single line for statement" $ do
      let source = "for (val i: Size = 0; i < 10; i++): len++"
      parseAST pFor source `shouldParse` ForUD
        (DeclareUD "i" (TyDef "Size") (Just (LiteralUD (IntLiteral 0 Dec []))))
        (BinaryUD LessThan (IdentifierUD "i") (LiteralUD (IntLiteral 10 Dec [])))
        (UnaryUD UnaryPostfix Increment (IdentifierUD "i"))
        [SExprUD (UnaryUD UnaryPostfix Increment (IdentifierUD "len"))]

    it "parses a multiline for statement" $ do
      let source = deindent [r|
        for (val i: Size = 0; i < 10; i++):
          len++
      |]
      parseAST pFor source `shouldParse` ForUD
        (DeclareUD "i" (TyDef "Size") (Just (LiteralUD (IntLiteral 0 Dec []))))
        (BinaryUD LessThan (IdentifierUD "i") (LiteralUD (IntLiteral 10 Dec [])))
        (UnaryUD UnaryPostfix Increment (IdentifierUD "i"))
        [SExprUD (UnaryUD UnaryPostfix Increment (IdentifierUD "len"))]

  describe "expressions" $ do
    it "parses a simple expression into a statement" $ do
      let source = "1"
      parseAST pStmtExpr source `shouldParse` SExprUD (LiteralUD (IntLiteral 1 Dec []))

    it "parses an assignment expression" $ do
      let source = "A = 1"
      parseAST pStmtExpr source `shouldParse` SExprUD (AssignUD (IdentifierUD "A")
        (LiteralUD (IntLiteral 1 Dec [])))

