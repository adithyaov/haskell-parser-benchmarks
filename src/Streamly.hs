module Streamly (parseFile) where

import Control.Applicative ((<|>))
import Data.Either (fromRight)
import Data.Word (Word8)
import GHC.Base (unsafeChr)
import Streamly.Data.Array (Array)
import Streamly.Data.Parser (Parser)
import Streamly.Data.ParserK (ParserK)

import qualified Data.Char as Char
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.ParserK as ParserK
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.FileSystem.File as File
-- import qualified Streamly.Unicode.Stream as Unicode

import Expr

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

{-# INLINE char #-}
char :: Char -> Parser Word8 IO ()
char c = fmap (const ()) $ Parser.satisfy (== fromIntegral (Char.ord c))

{-# INLINE decimal #-}
decimal :: Integral a => Parser Word8 IO a
decimal = Parser.takeWhile1 (Char.isDigit . unsafeChr . fromIntegral) (Fold.foldl' step 0)

    where

    step a c = a * 10 + fromIntegral (c - 48)

-- | Drop /zero/ or more white space characters.
{-# INLINE dropSpace #-}
dropSpace :: Parser Word8 IO ()
dropSpace = Parser.takeWhile (Char.isSpace . unsafeChr. fromIntegral) Fold.drain

{-# INLINE lexeme #-}
lexeme :: Parser Word8 IO b -> Parser Word8 IO b
lexeme p = p <* dropSpace

--------------------------------------------------------------------------------
-- ParserK
--------------------------------------------------------------------------------

{-# INLINE chainl1 #-}
chainl1 :: ParserK (Array b) IO a -> ParserK (Array b) IO (a -> a -> a) -> ParserK (Array b) IO a
chainl1 p op = p >>= go

    where

    go l =
        step l <|> pure l
    step l = do
        c <- op
        r <- p
        go (c l r)

{-# INLINE atom #-}
atom :: ParserK (Array Word8) IO Expr
atom =
    Num <$> ParserK.adaptC (lexeme decimal)
        <|> (  ParserK.adaptC (char '('))
            *> expr
            <* ParserK.adaptC (lexeme (char ')')
            )

{-# INLINE prod #-}
prod :: ParserK (Array Word8) IO Expr
prod = chainl1 atom (Bin <$> op)

    where

    op =
          ParserK.adaptC
        $ lexeme
        $ (Mul <$ char '*') <|> (Div <$ char '/')

{-# INLINE expr #-}
expr :: ParserK (Array Word8) IO Expr
expr = chainl1 prod (Bin <$> op)

    where

    op =
          ParserK.adaptC
        $ lexeme
        $ (Add <$ char '+') <|> (Sub <$ char '-')

parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    r <-
          StreamK.parseChunks expr
        $ StreamK.fromStream
        -- $ Unicode.decodeUtf8Chunks
        $ File.readChunks filepath
    return (Just (fromRight undefined r))
