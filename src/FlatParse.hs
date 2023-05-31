{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module FlatParse (parseFile, parseString) where

import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.Void
import FlatParse.Basic qualified as F

import Data.Function ((&))
import Expr


type Parser = F.Parser Void


{-# INLINE expr #-}
expr :: Parser Expr
expr = chainl1 prod (Bin <$> op)
    where
        op = lexeme $(F.switch [|case _ of "+" -> pure Add; "-" -> pure Sub|])


{-# INLINE prod #-}
prod :: Parser Expr
prod = chainl1 atom (Bin <$> op)
    where
        op = lexeme $(F.switch [|case _ of "*" -> pure Mul; "/" -> pure Div|])


{-# INLINE atom #-}
atom :: Parser Expr
atom =
    (Num . fromIntegral <$> lexeme F.anyAsciiDecimalWord)
        F.<|> (lexeme $(F.char '(') *> expr <* lexeme $(F.char ')'))


{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme p = p <* F.skipMany (F.skipSatisfy isSpace)


{-# INLINE chainl1 #-}
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= go
    where
        go l =
            step l F.<|> pure l
        step l = do
            c <- op
            r <- p
            go (c l r)


parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    content <- BS.readFile filepath
    pure $
        F.runParser (expr <* F.eof) content & \case
            F.OK ans _ -> Just ans
            _ -> Nothing


parseString :: String -> Maybe Expr
parseString str =
    case F.runParser (expr <* F.eof) (BS.pack str) of
        F.OK ans _ -> Just ans
        _ -> Nothing
