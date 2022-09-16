{-# LANGUAGE ScopedTypeVariables #-}

module Streamly (parseFile) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Catch (catch)
import GHC.Exception.Type (SomeException)
import Streamly.Internal.Data.Parser (Parser)
-- import Streamly.Internal.Data.Parser.ParserD (Parser)
import Streamly.Internal.Data.Parser.ParserD (Initial(..), Step(..))

import qualified Data.Char as Char
-- import qualified Streamly.Internal.Data.Parser.ParserD as Parser
-- import qualified Streamly.Internal.Data.Parser.ParserD as D
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.File as File
-- import qualified Streamly.Internal.Unicode.Char.Parser as Parser
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Expr

data Chainl1DState a s1 s2
    = CSInit s1
    | CSWithVal0 a s2
    | CSWithOP0 a (a -> a -> a) s1
    | CSWithVal Int a s2
    | CSWithOP Int a (a -> a -> a) s1

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

{-
-- XXX Implement takePGE
{-# INLINE chainl1D #-}
chainl1D ::
       (Show b, Show a)
    => D.Parser IO a b
    -> D.Parser IO a (b -> b -> b)
    -> D.Parser IO a b
chainl1D (D.Parser stp ini ext) (D.Parser stpOP iniOP _) =
    D.Parser step initial extract

    where

    parseValInit _Partial _Error = do
        res <- ini
        case res of
            IPartial s -> return $ _Partial $ CSInit s
            IDone b -> parseOP0 b _Partial _Error
            IError err -> return $ _Error err

    parseOP0 b _Partial _Error = do
        resOP <- iniOP
        case resOP of
            IPartial sOP -> return $ _Partial $ CSWithVal0 b sOP
            IDone op -> parseVal0 b op _Partial _Error
            IError err -> return $ _Error err

    parseVal0 b op _Partial _Error = do
        res <- ini
        case res of
            IPartial s -> return $ _Partial $ CSWithOP0 b op s
            IDone b1 -> parseOP1 0 (op b b1) _Partial _Error
            IError err -> return $ _Error err

    parseVal1 cnt b op _Partial _Error = do
        res <- ini
        case res of
            IPartial s -> return $ _Partial $ CSWithOP cnt b op s
            IDone b1 -> parseOP1 cnt (op b b1) _Partial _Error
            IError err -> return $ _Error err

    parseOP1 cnt b _Partial _Error = do
        resOP <- iniOP
        case resOP of
            IPartial sOP -> return $ _Partial $ CSWithVal cnt b sOP
            IDone op -> parseVal1 cnt b op _Partial _Error
            IError err -> return $ _Error err

    initial = parseValInit IPartial IError

    step (CSInit s) a = do
        print "CSInit"
        putStrLn $ "a: " ++ show a
        res <- stp s a
        case res of
            Partial n s1 -> return $ Continue n (CSInit s1)
            Continue n s1 -> return $ Continue n (CSInit s1)
            Done n b -> do
                putStrLn $ "n: " ++ show n
                parseOP0 b (Continue n) Error
            Error err -> return $ Error err
    step (CSWithVal0 b s) a = do
        print "CSWithVal0"
        putStrLn $ "b: " ++ show b
        putStrLn $ "a: " ++ show a
        res <- stpOP s a
        case res of
            Partial n s1 -> return $ Continue n (CSWithVal0 b s1)
            Continue n s1 -> return $ Continue n (CSWithVal0 b s1)
            Done n op -> do
                putStrLn $ "nn: " ++ show n
                parseVal0 b op (Continue n) Error
            Error err -> return $ Error $ show a
    step (CSWithOP0 b op s) a = do
        print "CSWithOP0"
        print a
        res <- stp s a
        case res of
            Partial n s1 -> return $ Continue n (CSWithOP0 b op s1)
            Continue n s1 -> return $ Continue n (CSWithOP0 b op s1)
            Done n b1 ->
                let b2 = op b b1
                 in parseOP1 0 b2 (Partial n) (\_ -> Done 0 b2)
            Error err -> return $ Error err
    step (CSWithVal cnt0 b s) a = do
        print "CSWithVal"
        print a
        print b
        let cnt = cnt0 + 1
        res <- stpOP s a
        case res of
            Partial n s1 -> return $ Partial n (CSWithVal (cnt - n) b s1)
            Continue n s1 -> return $ Partial n (CSWithVal (cnt - n) b s1)
            Done n op -> parseVal1 (cnt - n) b op (Partial n) (\_ -> Done n b)
            Error _ -> return $ Done cnt b
    step (CSWithOP cnt0 b op s) a = do
        print "CSWithOP"
        print b
        print a
        let cnt = cnt0 + 1
        res <- stp s a
        case res of
            Partial n s1 -> do
                print n
                return $ Partial n (CSWithOP (cnt - n) b op s1)
            Continue n s1 -> return $ Partial n (CSWithOP (cnt - n) b op s1)
            Done n b1 -> do
                let b2 = op b b1
                parseOP1 (cnt - n) b2 (Partial n) (\_ -> Done n b2)
            Error _ -> return $ Done cnt b

    extract (CSInit _) = return $ Error "Insufficient input 1"
    extract (CSWithVal0 _ _) = return $ Error "Insufficient input 2"
    extract (CSWithOP0 b op s) = do
        res <- ext s
        case res of
            Partial _ _ -> error "Unreachable branch"
            Continue _ _ -> error "Unreachable branch"
            Done n b1 -> return $ Done n (op b b1)
            Error _ -> return $ Error "Insufficient input"
    extract (CSWithVal i b _) = return $ Done i b
    extract (CSWithOP i b _ _) = return $ Done i b

{-# INLINE chainl1 #-}
chainl1 ::
       (Show b, Show a)
    => Parser IO a b
    -> Parser IO a (b -> b -> b)
    -> Parser IO a b
chainl1 psr psrOP =
    D.toParserK $ chainl1D (D.fromParserK psr) (D.fromParserK psrOP)
-}

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

{-# INLINE chainl1 #-}
chainl1 :: Parser IO b a -> Parser IO b (a -> a -> a) -> Parser IO b a
chainl1 p op = do
    x <- p
    rest x

    where

    rest x =
        (do f <- op
            y <- p
            rest (f x y))
            <|> return x

{-# INLINE char #-}
char :: Char -> Parser IO Char ()
char c = fmap (const ()) $ satisfy (== c)

{-# INLINE decimal #-}
decimal :: Integral a => Parser IO Char a
decimal = takeWhile1 Char.isDigit (Fold.foldl' step 0)

    where

    step a c = a * 10 + fromIntegral (Char.ord c - 48)

-- | Drop /zero/ or more white space characters.
{-# INLINE dropSpace #-}
dropSpace :: Parser IO Char ()
dropSpace = takeWhile Char.isSpace Fold.drain

{-# INLINE expr #-}
expr :: Parser IO Char Expr
expr = chainl1 prod (Bin <$> op)
    where
        op = lexeme $ Add <$ char '+' <|> Sub <$ char '-'

{-# INLINE prod #-}
prod :: Parser IO Char Expr
prod = chainl1 atom (Bin <$> op)
    where
        op = lexeme $ Mul <$ char '*' <|> Div <$ char '/'

{-# INLINE atom #-}
atom :: Parser IO Char Expr
atom =
    Num <$> lexeme decimal
        <|> lexeme (char '(') *> expr <* lexeme (char ')')

{-# INLINE lexeme #-}
lexeme :: Parser IO Char b -> Parser IO Char b
lexeme p = p <* dropSpace

{-
parseString :: [Char] -> IO ()
parseString = (=<<) print . Stream.parse expr . Stream.fromList

    where

    psr =
        lexeme decimal *> lexeme (Parser.char '*')
            *> lexeme decimal
-}

parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    let action =
            Stream.parseK expr
                $ Unicode.decodeLatin1 $ Stream.unfold File.read filepath
    catch (Just <$> action) (\(_ :: SomeException) -> return (Just (Num 0)))
