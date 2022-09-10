{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}

module Megaparsec where

import Control.Applicative
import Data.Char (isSpace)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

import Expr


type Parser = Parsec () T.Text


{-# INLINE expr #-}
expr :: Parser Expr
expr = chainl1 prod (Bin <$> op)
    where
        op = lexeme $ Add <$ char '+' <|> Sub <$ char '-'


{-# INLINE prod #-}
prod :: Parser Expr
prod = chainl1 atom (Bin <$> op)
    where
        op = lexeme $ Mul <$ char '*' <|> Div <$ char '/'


{-# INLINE atom #-}
atom :: Parser Expr
atom = lexeme $ Num <$> decimal


{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme p = p <* takeWhileP Nothing isSpace


{-# INLINE chainl1 #-}
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= go
    where
        go l =
            step l <|> pure l
        step l = do
            c <- op
            r <- p
            go (c l r)


parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    content <- T.readFile filepath
    pure $ parseMaybe expr content
