module SeqSel.Parser (Var(..), Expr(..), parseFile, name) where

import Prelude hiding (sequence)
import Text.Parsec
import Text.Parsec.String
import Data.String.Utils

data Var = Var String
            deriving (Eq, Show)

data Expr = Selector String [Expr]
          | Sequence String [Expr]
          | Condition String
          | Call String
            deriving (Eq, Show)

type Defines = [(String, String)]

name :: Expr -> String
name (Selector n _) = n
name (Sequence n _) = n

atom :: Parser String
atom = (:) <$> lower <*> many (alphaNum <|> oneOf "_")

rest :: Defines -> Parser String
rest defs = do
    str <- many (noneOf "\n")
    let str' = foldl (\s (from, to) -> replace from to s) str defs
    skipMany newline
    return (str')

rest' :: Parser String
rest' = rest []

dropstring :: String -> Parser ()
dropstring s = string s *> pure ()

define :: Parser (String, String)
define = do
    dropstring "#define"
    spaces
    key <- atom
    spaces
    value <- rest'
    skipMany newline
    return ('#':key, value)

var :: Parser Var
var = do
    dropstring "#var"
    spaces
    v <- rest'
    skipMany newline
    return (Var v)

nodeName :: Parser String
nodeName = (:) <$> upper <*> many letter <* char ':' <* skipMany newline 

selector :: Int -> Defines -> Parser Expr
selector indent defs = do
    dropstring "selector"
    spaces
    n <- nodeName
    skipMany newline
    children <- many1 (expr (indent + 1) defs)
    skipMany newline
    return (Selector n children)

sequence :: Int -> Defines -> Parser Expr
sequence indent defs = do
    dropstring "sequence"
    spaces
    n <- nodeName
    skipMany newline
    children <- many1 (expr (indent + 1) defs)
    skipMany newline
    return (Sequence n children)

cond :: Defines -> Parser Expr
cond defs = Condition <$> (string "cond" *> spaces *> rest defs)

call :: Defines -> Parser Expr
call defs = Call <$> (string "call" *> spaces *> rest defs)

ts :: Int -> Parser ()
ts indent = count indent (string "\t" <|> string "    ") *> pure ()

expr :: Int -> Defines -> Parser Expr
expr indent defs = do
    try (ts indent)
    (try (selector indent defs)
      <|> sequence indent defs
      <|> try (cond defs)
      <|> call defs)

file :: Parser ([Var], Expr)
file = do
    defs <- many (try define)
    vars <- many (try var)
    tree <- expr 0 defs
    return (vars, tree)

parseFile :: FilePath -> IO (Either ParseError ([Var], Expr))
parseFile fname = do
    input <- readFile fname
    return (runParser file () fname input)
