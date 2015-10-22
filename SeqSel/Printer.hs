{-# LANGUAGE OverloadedStrings #-}
module SeqSel.Printer (printExpr, printVar, printFunc, printTree) where

import Data.Monoid
import qualified Data.Text.Lazy as L
import SeqSel.Parser

indent :: Int -> L.Text
indent n = L.pack (replicate n ' ')

printExpr :: Int -> Expr -> L.Text
printExpr n (Selector node children) = "\n" <> indent n <> "/* " <> L.pack node
                                    <> " */\n" <> indent n <> "(" <> children'
                                    <> "\n" <> indent n <> ")"
  where
    children' = L.intercalate ("\n" <> indent n <> "|| ")
                              (map (printExpr (n + 4)) children)

printExpr n (Sequence node children) = "\n" <> indent n <> "/* " <> L.pack node
                                    <> " */\n" <> indent n <> "(" <> children' <> ")"
  where
    children' = L.intercalate " && " (map (printExpr (n + 4)) children)

printExpr _ (Condition cond) = L.pack cond

printExpr _ (Call fun) = L.pack fun

printVar :: Var -> L.Text
printVar (Var str) = L.pack str <> ";\n"

printFunc :: Expr -> L.Text
printFunc expr = indent 4 <> "bool run()\n" <> indent 4 <> "{\n"
              <> indent 8 <> "return" <> printExpr 8 expr <> ";\n" <> indent 4 <> "}"

printTree :: [Var] -> Expr -> L.Text
printTree vars expr = "#pragma once\n\nclass "
                   <> L.pack (name expr) <> "\n{\npublic:" <> vars'
                   <> "\n\n" <> printFunc expr <> "\n};"
  where
    vars' = L.concat $ map (\(Var v) -> "\n" <> indent 4 <> L.pack v <> ";") vars