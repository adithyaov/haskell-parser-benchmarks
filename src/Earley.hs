{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecursiveDo #-}

module Earley (parseFile, parseString, grammar) where

import Control.Applicative
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, ord)
import Data.ListLike (CharString (..))
import Data.Void (Void)

import Text.Earley

import Expr


grammar :: Grammar r (Prod r Void Char Expr)
grammar = mdo
    parOpen <- rule $ token '('
    parClose <- rule $ token ')'
    sumOp <- rule $ Add <$ token '+' <|> Sub <$ token '-'
    prodOp <- rule $ Mul <$ token '*' <|> Div <$ token '/'
    digit <- rule $ (\c -> fromIntegral (ord c - ord '0')) <$> satisfy isDigit
    number <- rule $ (\ !q !r -> 10 * q + r) <$> number <*> digit <|> digit

    sum' <-
        rule $
            flip Bin <$> sum' <*> sumOp <*> prod
                <|> prod

    prod <-
        rule $
            flip Bin <$> prod <*> prodOp <*> atom
                <|> atom

    atom <-
        rule $ (Num <$> number) <|> parOpen *> sum' <* parClose

    pure sum'


parsingGrammar :: Parser Void CharString Expr
parsingGrammar = parser grammar


parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    content <- CS <$> BS.readFile filepath
    pure $ case fullParses parsingGrammar content of
        ([ans], _) -> Just ans
        _ -> Nothing


parseString :: String -> Maybe Expr
parseString str = case fullParses parsingGrammar $ CS $ BS.pack str of
    ([ans], _) -> Just ans
    _ -> Nothing
