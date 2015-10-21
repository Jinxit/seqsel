module BTree.Parser (Statement, Expr, parseLine, parseFile) where

import Prelude hiding (sequence)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

data Statement = Statement Int Expr
            deriving (Eq, Show)

data Expr = Selector String
          | Sequence String
          | Condition String
          | Call String
          | Define String String
            deriving (Eq, Show)

atom :: Parser String
atom = (:) <$> lower <*> many (alphaNum <|> oneOf "_")

var :: Parser String
var = char '$' *> atom

defvar :: Parser String
defvar = char '#' *> atom

rest :: Parser String
rest = many (noneOf "\n")

define :: Parser Expr
define = do
    string "#define"
    spaces
    key <- atom
    spaces
    value <- rest
    return (Define key value)

name :: Parser String
name = (:) <$> upper <*> many letter <* char ':'

selector :: Parser Expr
selector = Selector <$> (string "selector" *> spaces *> name)

sequence :: Parser Expr
sequence = Sequence <$> (string "sequence" *> spaces *> name)

cond :: Parser Expr
cond = Condition <$> (string "cond" *> spaces *> rest)

call :: Parser Expr
call = Call <$> (string "call" *> spaces *> rest)

tabs :: Parser Int
tabs = do
    indent <- many (string "\t" <|> string "    ")
    return (length indent)

expr :: Parser Expr
expr = try define
   <|> try selector
   <|> sequence
   <|> try cond
   <|> call

statement :: Parser Statement
statement = do
    indent <- tabs
    expression <- expr
    skipMany newline
    return (Statement indent expression)

parseLines :: Parser [Statement]
parseLines = skipMany newline *> many1 statement

parseLine :: String -> Either ParseError Statement
parseLine = parse statement ""

parseFile :: FilePath -> IO (Either ParseError [Statement])
parseFile fname = do
    input <- readFile fname
    return (runParser parseLines () fname input)
