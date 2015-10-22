{-# LANGUAGE OverloadedStrings #-}
module SeqSel.Printer (printExpr) where

import Control.Monad.Writer
import qualified Data.Text.Lazy as L
import SeqSel.Parser

indent :: Int -> String
indent = flip replicate ' '

printExpr :: Expr -> L.Text
printExpr (Selector name children) = "/* " <> L.pack name <> " */ (" <> children' <> ")"
  where
    children' = L.intercalate " || " (map printExpr children)
printExpr (Sequence name children) = "/* " <> L.pack name <> " */ (" <> children' <> ")"
  where
    children' = L.intercalate " && " (map printExpr children)
printExpr (Condition cond) = L.pack cond
printExpr (Call fun) = L.pack fun