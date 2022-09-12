{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecursiveDo #-}

module Earley (parseFile) where

import Control.Applicative
import Data.ByteString qualified as BS
import Data.Char (isDigit, ord)
import Data.ListLike (CharString (..))
import Data.Void (Void)

import Text.Earley

import Expr


grammar :: Parser Void CharString Expr
grammar = parser $ mdo
    parOpen <- rule $ token '('
    parClose <- rule $ token ')'
    sumOp <- rule $ Add <$ token '+' <|> Sub <$ token '-'
    prodOp <- rule $ Mul <$ token '*' <|> Div <$ token '/'
    digit <- rule $ (\c -> fromIntegral (ord c) - 40) <$> satisfy isDigit
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


parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    content <- CS <$> BS.readFile filepath
    pure $ case fullParses grammar content of
        ([ans], _) -> Just ans
        _ -> Nothing
