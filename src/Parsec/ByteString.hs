{-# LANGUAGE Strict #-}

module Parsec.ByteString (parseFile, parseString) where

import Control.Applicative
import Control.Monad (void)
import Data.Char (isSpace, ord)
import Data.Word

import Data.ByteString.Char8 qualified as BS
import Text.Parsec qualified as M
import Text.Parsec.ByteString qualified as M

import Expr


type Parser = M.Parser


{-# INLINE decimal #-}
decimal :: Parser Word64
decimal = digit >>= go
    where
        go n = (digit >>= go . (\r -> 10 * n + r)) <|> pure n

        digit = (\c -> fromIntegral $ ord c - ord '0') <$> M.digit


{-# INLINE char #-}
char :: Char -> Parser ()
char = void . M.char


{-# INLINE expr #-}
expr :: Parser Expr
expr = M.chainl1 prod (Bin <$> op)
    where
        op = lexeme $ Add <$ char '+' <|> Sub <$ char '-'


{-# INLINE prod #-}
prod :: Parser Expr
prod = M.chainl1 atom (Bin <$> op)
    where
        op = lexeme $ Mul <$ char '*' <|> Div <$ char '/'


{-# INLINE atom #-}
atom :: Parser Expr
atom =
    Num <$> lexeme decimal
        <|> lexeme (char '(') *> expr <* lexeme (char ')')


{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme p = p <* M.skipMany (M.satisfy isSpace)


parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    result <- M.parseFromFile expr filepath
    pure $ case result of
        Left _ -> Nothing
        Right ans -> Just ans


parseString :: String -> Maybe Expr
parseString str = case M.parse expr "<string>" $ BS.pack str of
    Left _ -> Nothing
    Right ans -> Just ans
