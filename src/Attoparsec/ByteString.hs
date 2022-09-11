{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}

module Attoparsec.ByteString where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (isSpace)
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Function ((&))

import Expr


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
lexeme p = p <* skipWhile isSpace


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
    content <- BS.readFile filepath
    pure $
        parseOnly expr content & \case
            Left _ -> Nothing
            Right a -> Just a
