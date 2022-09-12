{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}

module Megaparsec.ByteString where

import Control.Applicative
import Control.Monad (void)
import Data.Char (chr, isSpace, ord)
import Data.Void
import Data.Word

import Data.ByteString qualified as BS
import Text.Megaparsec
import Text.Megaparsec.Byte qualified as M
import Text.Megaparsec.Byte.Lexer (decimal)

import Expr


type Parser = Parsec Void BS.ByteString


{-# INLINE c2w #-}
c2w :: Char -> Word8
c2w = fromIntegral . ord


{-# INLINE w2c #-}
w2c :: Word8 -> Char
w2c = chr . fromIntegral


{-# INLINE char #-}
char :: Char -> Parser ()
char = void . M.char . c2w


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
atom =
    Num <$> lexeme decimal
        <|> lexeme (char '(') *> expr <* lexeme (char ')')


{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme p = p <* takeWhileP Nothing (isSpace . w2c)


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
    pure $ parseMaybe expr content
