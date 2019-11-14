{-# LANGUAGE LambdaCase #-}
module Parser.Type where

import Data.Functor ((<&>))

import Text.Megaparsec

import AST
import Parser.Common

pUnaryType :: Parser Type
pUnaryType = cIdentifier <&> \case
  "Void"   -> TyVoid
  "Char"   -> TyChar
  "String" -> TyPtr TyChar
  n        -> TyDef n

pWrapType :: Parser Type
pWrapType = do
  outer <- symbol "Ptr"
  inner <- angles pType
  return $ TyPtr inner 

pType :: Parser Type
pType = pWrapType <|> pUnaryType