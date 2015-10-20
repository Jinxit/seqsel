import Prelude hiding (sequence)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

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

define :: Parser Expr
define = do
    string "#define"
    spaces
    key <- atom
    spaces
    value <- many anyChar
    return (Define key value)

name :: Parser String
name = (:) <$> upper <*> many letter <* char ':'

selector :: Parser Expr
selector = Selector <$> (string "selector" *> spaces *> name)

sequence :: Parser Expr
sequence = Sequence <$> (string "sequence" *> spaces *> name)

cond :: Parser Expr
cond = Condition <$> (string "cond" *> spaces *> many anyChar)

call :: Parser Expr
call = Call <$> (string "call" *> spaces *> many anyChar)

expr :: Parser Expr
expr = try define
   <|> try (selector <* eof)
   <|> sequence <* eof
   <|> try cond
   <|> call

main :: IO ()
main = do
    s <- getContents
    mapM_ putStrLn (map (show . (parse expr "")) (lines s))