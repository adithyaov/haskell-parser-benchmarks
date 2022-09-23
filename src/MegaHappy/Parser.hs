module MegaHappy.Parser (Parser, Token(..), parseToken) where

import Data.Void
import Data.Word
import Data.Char
import Data.ByteString (ByteString)
import Control.Applicative
import Text.Megaparsec qualified as P
import Text.Megaparsec.Byte qualified as P (char, space)
import Text.Megaparsec.Byte.Lexer qualified as P (decimal)

import Expr

type Parser = P.Parsec Void ByteString

parseToken :: (Token -> Parser a) -> Parser a
parseToken cont = do
  P.space
  isEof <- P.atEnd
  if isEof then cont TEndOfFile else pToken >>= cont

data Token
  = TOp !Op
  | TParBeg
  | TParEnd
  | TNum !Word64
  | TEndOfFile

pToken :: Parser Token
pToken =
    TOp Add <$ P.char (c2w '+') <|>
    TOp Sub <$ P.char (c2w '-') <|>
    TOp Mul <$ P.char (c2w '*') <|>
    TOp Div <$ P.char (c2w '/') <|>
    TParBeg <$ P.char (c2w '(') <|>
    TParEnd <$ P.char (c2w ')') <|>
    TNum <$> P.decimal

{-# INLINE c2w #-}
c2w :: Char -> Word8
c2w = fromIntegral . ord