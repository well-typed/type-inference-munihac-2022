module Parser where

import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Syntax

type Parser = Parsec Void String

sc :: Parser ()
sc =
  Lexer.space
    space1
    (Lexer.skipLineComment "--")
    (Lexer.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme =
  Lexer.lexeme sc

symbol :: String -> Parser String
symbol =
  Lexer.symbol sc

ident :: Parser String
ident = do
  i <- lexeme (takeWhile1P Nothing isAlpha)
  guard (not (i `elem` keywords))
  pure i

keywords :: [String]
keywords = ["let", "in"]

sym :: String -> Parser String
sym =
  Lexer.symbol sc

keyw :: String -> Parser String
keyw =
  Lexer.symbol sc

parens :: Parser a -> Parser a
parens p =
  sym "(" *> p <* sym ")"

int :: Parser Integer
int =
  Lexer.decimal

expr :: Parser Expr
expr =
  foldl1 App <$> some term

term :: Parser Expr
term =
      Var <$> ident
  <|> lam <$ (sym "\\" <?> "lambda") <*> some ident <* sym "->" <*> expr
  <|> letfun <$ keyw "let" <*> ident <*> many ident <* sym "=" <*> expr
             <* keyw "in"  <*> expr
  <|> parens expr

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr xs =
  parse (sc *> expr <* eof) "interactive" xs

decl :: Parser (Name, Expr)
decl =
     (\ x xs e -> (x, lam xs e))
  <$  keyw "let"
  <*> ident
  <*> many ident
  <*  sym "="
  <*> expr

toplevel :: Parser (Name, Expr)
toplevel =
      ((,) "it")
  <$> try expr
  <|> decl

parseToplevel :: String -> Either (ParseErrorBundle String Void) (String, Expr)
parseToplevel =
  parse (sc *> toplevel <* eof) "interactive"
