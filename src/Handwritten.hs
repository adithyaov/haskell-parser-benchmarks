{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Handwritten (parseFile, main, parse, expr, Pos (..)) where

import Control.Exception (evaluate)
import Control.Monad ((>=>))
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isSpace, ord)
import Data.Foldable (traverse_)
import Data.Word (Word64)
import System.Environment (getArgs)

import Expr


main :: IO ()
main =
    getArgs >>= traverse_ (parseFile >=> evaluate)


parseFile :: FilePath -> IO (Maybe Expr)
parseFile path = parse expr <$> BS.readFile path


data Token
    = TInt Word64
    | TOp Op
    | TParBeg
    | TParEnd
    | TEndOfFile


data LexerState
    = S
        Token
        {-# UNPACK #-} Pos
        {-# UNPACK #-} BS.ByteString


data Pos = Pos
    { line :: {-# UNPACK #-} Word64
    , column :: {-# UNPACK #-} Word64
    }


incLine :: Pos -> Pos
incLine (Pos l c) = Pos (l + 1) c


incColumnBy :: Int -> Pos -> Pos
incColumnBy dc (Pos l c) = Pos l (c + fromIntegral dc)


{-# INLINE nextTok #-}
nextTok :: Pos -> BS.ByteString -> Maybe LexerState
nextTok pos input =
    let (ws, rest) = BS.span isSpace input
        pos' = BS.foldl' updatePos pos ws
     in case BS.uncons rest of
            Nothing -> Just $ S TEndOfFile pos' BS.empty
            Just (c, rest') -> case c of
                '+' -> Just $ S (TOp Add) (incColumnBy 1 pos') rest'
                '-' -> Just $ S (TOp Sub) (incColumnBy 1 pos') rest'
                '*' -> Just $ S (TOp Mul) (incColumnBy 1 pos') rest'
                '/' -> Just $ S (TOp Div) (incColumnBy 1 pos') rest'
                '(' -> Just $ S (TParBeg) (incColumnBy 1 pos') rest'
                ')' -> Just $ S (TParEnd) (incColumnBy 1 pos') rest'
                _ | isDigit c -> lexNum pos' rest' $ digit c
                _ -> Nothing
    where
        lexNum pos' input' n =
            let (num_str, rest) = BS.span isDigit input'
                num = BS.foldl' (\k c -> 10 * k + digit c) n num_str
             in Just $ S (TInt num) (incColumnBy (1 + BS.length num_str) pos') rest

        digit c = fromIntegral $ ord c - ord '0'
        updatePos p '\n' = incLine p
        updatePos p _ = incColumnBy (1 :: Int) p


newtype Lexer a
    = Lexer
        ( forall r.
          LexerState ->
          (LexerState -> r) ->
          (LexerState -> a -> r) ->
          r
        )


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
runLexer m bs = case nextTok (Pos 0 0) bs of
    Nothing -> Nothing
    Just (S tok pos inp) -> unLexer m (S tok pos inp) (const Nothing) (\_ a -> Just a)


parse :: Lexer a -> BS.ByteString -> Maybe a
parse = runLexer


instance Monad Lexer where
    {-# INLINE (>>=) #-}
    m >>= f = Lexer $ \s r g -> unLexer m s r $ \s' a -> unLexer (f a) s' r g


instance Applicative Lexer where
    {-# INLINE pure #-}
    pure a = Lexer $ \s _ g -> g s a


    {-# INLINE (<*>) #-}
    mf <*> ma =
        Lexer $ \s r g ->
            unLexer mf s r $ \s' f ->
                unLexer ma s' r $ \s'' -> g s'' . f


    {-# INLINE (*>) #-}
    mx *> ma = Lexer $ \s r g -> unLexer mx s r $ \s' _ -> unLexer ma s' r g


instance Functor Lexer where
    {-# INLINE fmap #-}
    fmap f m = Lexer $ \r s g -> unLexer m r s (\s' -> g s' . f)


{-# INLINE empty #-}
empty :: Lexer a
empty = Lexer $ \s r _ -> r s


{-# INLINE peek #-}
peek :: Lexer Token
peek = Lexer $ \s@(S t _ _) _ c -> c s t


{-# INLINE pop #-}
pop :: Lexer Token
pop = Lexer $ \s@(S tok pos input) r c -> case nextTok pos input of
    Nothing -> r s
    Just s' -> c s' tok


{-# INLINE expr #-}
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
        TInt n -> pure $ Num n
        _ -> empty


sum' :: Expr -> Lexer Expr
sum' = go
    where
        go !l =
            peek >>= \case
                TOp Add -> do
                    _ <- pop
                    a <- atom
                    r <- prod a
                    go $ Bin Add l r
                TOp Sub -> do
                    _ <- pop
                    a <- atom
                    r <- prod a
                    go $ Bin Sub l r
                _ -> pure l


prod :: Expr -> Lexer Expr
prod = go
    where
        go !l =
            peek >>= \case
                TOp Mul -> do
                    _ <- pop
                    r <- atom
                    go $ Bin Mul l r
                TOp Div -> do
                    _ <- pop
                    r <- atom
                    go $ Bin Div l r
                _ -> pure l
