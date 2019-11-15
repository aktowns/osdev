{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.TypeSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.RawString.QQ
import Text.Megaparsec (parse)

import Parser.Type
import AST

import Parser.Utils

spec :: Spec
spec =
  describe "type" $ do
    it "parses a type" $ do
      let source = "UInt"
      parseAST' pType source `shouldParse` TyDef "UInt"
    it "parses pointers" $ do
      let source = "Ptr<SomeType>"
      parseAST' pType source `shouldParse` TyPtr (TyDef "SomeType")
