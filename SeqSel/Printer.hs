{-# LANGUAGE OverloadedStrings #-}
module SeqSel.Printer (printExpr, printVar, printRoot) where

import Data.Monoid
import qualified Data.Text.Lazy as L
import SeqSel.Parser

indent :: Int -> L.Text
indent n = L.pack (replicate n ' ')

printExpr :: Int -> Expr -> L.Text
printExpr n (Selector node children) = "\n" <> indent n <> "/* " <> L.pack node
                                    <> " */\n" <> indent n <> "(" <> children' <> "\n" <> indent n <> ")"
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

printRoot :: Expr -> L.Text
printRoot expr = "bool " <> L.pack (name expr) <> "()\n{\n"
              <> indent 4 <> "return" <> printExpr 12 expr <> ";\n}"
