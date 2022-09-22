{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Handwritten.Normal (parseFile, Pos (..)) where

import Control.Monad.Trans.State.Strict
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, ord)
import Data.Word (Word64)

import Expr


parseFile :: FilePath -> IO (Maybe Expr)
parseFile path = parse expr <$> BS.readFile path


data Token where
    T
        :: TokenTag a
        -> a
        -> {-# UNPACK #-} Pos
        -> Token


data TokenTag a where
    TInt :: TokenTag Word64
    TOp :: TokenTag Op
    TParBeg :: TokenTag ()
    TParEnd :: TokenTag ()
    TEndOfFile :: TokenTag ()


data LexerState
    = S
        Token
        {-# UNPACK #-} BS.ByteString


data Pos = Pos
    { line :: {-# UNPACK #-} Word64
    , column :: {-# UNPACK #-} Word64
    }


newtype Lexer a
    = Lexer { unLexer :: LexerState -> Maybe (a, LexerState) }
    deriving (Functor, Applicative, Monad, MonadFail) via StateT LexerState Maybe

runLexer :: Lexer a -> BS.ByteString -> Maybe a
runLexer m input =
    let initialState = S (T TEndOfFile () (Pos 0 0)) input
     in fst <$> unLexer (pop *> m) initialState


parse :: Lexer a -> BS.ByteString -> Maybe a
parse = runLexer


empty :: Lexer a
empty = Lexer $ const Nothing


peek :: Lexer Token
peek = Lexer $ \s@(S t _)  -> Just (t, s)


updatePos :: Char -> Pos -> Pos
updatePos '\n' (Pos l _) = Pos (l + 1) 0
updatePos _ (Pos l c) = Pos l (c + 1)


{-# INLINE popToken #-}
popToken :: LexerState -> Maybe (Token, LexerState)
popToken (S t@(T _ _ p) b) = go p b
    where
        go pos bs = case BS.uncons bs of
            Nothing -> Just (t, S (T TEndOfFile () pos) bs)
            Just (c, cs) ->
                let pos' = updatePos c pos
                 in case c of
                        '+' -> Just (t, S (T TOp Add pos') cs)
                        '-' -> Just (t, S (T TOp Sub pos') cs)
                        '*' -> Just (t, S (T TOp Mul pos') cs)
                        '/' -> Just (t, S (T TOp Div pos') cs)
                        '(' -> Just (t, S (T TParBeg () pos') cs)
                        ')' -> Just (t, S (T TParEnd () pos') cs)
                        ' ' -> go pos' cs
                        '\n' -> go pos' cs
                        _ | isDigit c -> goNum pos' cs $ digit c
                        _ -> Nothing
        goNum pos bs n = case BS.uncons bs of
            Nothing -> Just (t, S (T TInt n pos) bs)
            Just (c, cs)
                | isDigit c -> goNum (updatePos c pos) cs (10 * n + digit c)
                | otherwise -> Just (t, S (T TInt n pos) bs)
        digit c = fromIntegral $ ord c - ord '0'


pop :: Lexer Token
pop = Lexer popToken


expr :: Lexer Expr
expr = do
    a <- atom
    p <- prod a
    sum' p


{-# INLINE atom #-}
atom :: Lexer Expr
atom =
    pop >>= \case
        T TInt n _ -> pure $ Num n
        T TParBeg _ _ -> do
            e <- expr
            T TParEnd _ _ <- pop
            pure e
        _ -> empty


sum' :: Expr -> Lexer Expr
sum' !l =
    peek >>= \case
        T TOp Add _ -> do
            _ <- pop
            a <- atom
            r <- prod a
            sum' $ Bin Add l r
        T TOp Sub _ -> do
            _ <- pop
            a <- atom
            r <- prod a
            sum' $ Bin Sub l r
        _ -> pure l


prod :: Expr -> Lexer Expr
prod !l =
    peek >>= \case
        T TOp Mul _ -> do
            _ <- pop
            r <- atom
            prod $ Bin Mul l r
        T TOp Div _ -> do
            _ <- pop
            r <- atom
            prod $ Bin Div l r
        _ -> pure l
