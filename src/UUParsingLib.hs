module UUParsingLib (parseFile) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Word (Word64)
import Data.Char (isSpace, isDigit, ord)

import Text.ParserCombinators.UU ( pEnd, parse_h, pChainl, P, (<<|>), (<|>) )
import Text.ParserCombinators.UU.BasicInstances (pSym, createStr, pMunch, Str)

import Expr ( Expr(..), Op(Div, Add, Sub, Mul) )
import Data.List (foldl')


type Parser = P (Str Char T.Text Int)

char :: Char -> Parser Char
char = pSym

decimal :: Parser Word64
decimal = foldl' (\a b -> a * 10 + fromIntegral (ord b) - 48) 0 <$> pMunch isDigit

pSpaces :: Parser String
pSpaces = pMunch isSpace

lexeme :: Parser a -> Parser a
lexeme p = p <* pSpaces

expr :: Parser Expr
expr = pChainl (Bin <$> lexeme op) prod
    where
        op = Add <$ char '+' <|> Sub <$ char '-'

prod :: Parser Expr
prod = pChainl (Bin <$> lexeme op) atom
    where
        op = Mul <$ char '*' <|> Div <$ char '/'

atom :: Parser Expr
atom =
   lexeme (char '(') *> expr <* lexeme (char ')')
     <<|> Num <$> lexeme decimal

parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    content <- T.readFile filepath
    pure $ case parse_h ((,) <$> expr <*> pEnd) (createStr 0 content) of
             (x, []) -> Just x
             _ -> Nothing
