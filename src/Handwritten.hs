{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Handwritten (parseFile, Pos (..)) where

import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, ord)
import Data.Word (Word64)

import Expr


parseFile :: FilePath -> IO (Maybe Expr)
parseFile path = parse expr <$> BS.readFile path


data Token where
    T ::
        TokenTag a ->
        a ->
        {-# UNPACK #-} Pos ->
        Token


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
    = Lexer
        ( forall r.
          LexerState ->
          (LexerState -> r) ->
          (LexerState -> a -> r) ->
          r
        )


{-# INLINE unLexer #-}
unLexer ::
    Lexer a ->
    forall r.
    -- | current state
    LexerState ->
    -- | failure
    (LexerState -> r) ->
    -- | success
    (LexerState -> a -> r) ->
    r
unLexer (Lexer f) = f


runLexer :: Lexer a -> BS.ByteString -> Maybe a
runLexer m input =
    let initialState = S (T TEndOfFile () (Pos 0 0)) input
     in unLexer (pop *> m) initialState (const Nothing) (const Just)


parse :: Lexer a -> BS.ByteString -> Maybe a
parse = runLexer


instance Monad Lexer where
    m >>= f =
        Lexer $ \s r g ->
            unLexer m s r $ \s' a ->
                unLexer (f a) s' r g


    (>>) = (*>)


instance Applicative Lexer where
    pure a = Lexer $ \s _ g -> g s a


    mf <*> ma =
        Lexer $ \s r g ->
            unLexer mf s r $ \s' f ->
                unLexer ma s' r $ \s'' -> g s'' . f


    mx *> ma =
        Lexer $ \s r g ->
            unLexer mx s r $ \s' _ ->
                unLexer ma s' r g


instance Functor Lexer where
    fmap f m =
        Lexer $ \r s g ->
            unLexer m r s $ \s' ->
                g s' . f


instance MonadFail Lexer where
    fail _ = empty


empty :: Lexer a
empty = Lexer $ \s r _ -> r s


peek :: Lexer Token
peek = Lexer $ \s@(S t _) _ c -> c s t


updatePos :: Char -> Pos -> Pos
updatePos '\n' (Pos l _) = Pos (l + 1) 0
updatePos _ (Pos l c) = Pos l (c + 1)


{-# INLINE popToken #-}
popToken :: LexerState -> (LexerState -> r) -> (LexerState -> Token -> r) -> r
popToken s@(S t@(T _ _ p) b) err ok = go p b
    where
        go pos bs = case BS.uncons bs of
            Nothing -> ok (S (T TEndOfFile () pos) bs) t
            Just (c, cs) ->
                let pos' = updatePos c pos
                 in case c of
                        '+' -> ok (S (T TOp Add pos') cs) t
                        '-' -> ok (S (T TOp Sub pos') cs) t
                        '*' -> ok (S (T TOp Mul pos') cs) t
                        '/' -> ok (S (T TOp Div pos') cs) t
                        '(' -> ok (S (T TParBeg () pos') cs) t
                        ')' -> ok (S (T TParEnd () pos') cs) t
                        ' ' -> go pos' cs
                        '\n' -> go pos' cs
                        _ | isDigit c -> goNum pos' cs $ digit c
                        _ -> err s
        goNum pos bs n = case BS.uncons bs of
            Nothing -> ok (S (T TInt n pos) bs) t
            Just (c, cs)
                | isDigit c -> goNum (updatePos c pos) cs (10 * n + digit c)
                | otherwise -> ok (S (T TInt n pos) bs) t
        digit c = fromIntegral $ ord c - ord '0'


pop :: Lexer Token
pop = Lexer popToken


expr :: Lexer Expr
expr = do
    a <- atom
    p <- prod a
    e <- sum' p
    pure e


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
