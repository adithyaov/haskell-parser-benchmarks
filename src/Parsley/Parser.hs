{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}


#define QQ(x) (makeQ (x) [|| x ||])

module Parsley.Parser (expr) where

import Prelude hiding ((*>), (<$), (<$>), (<*), (<*>))

import Expr

import Data.Char (ord)
import "parsley" Parsley
import Parsley.Char (oneOf)
import Parsley.Combinator (eof)
import Parsley.Defunctionalized (Defunc (LIFTED))
import Parsley.Fold (skipMany, somel)
import Parsley.Precedence (Fixity (InfixL), ops, precHomo)


expr :: Parser Expr
expr = whitespace *> sum' <* eof
    where
        whitespace = skipMany (oneOf " \n")
        lexeme p = p <* whitespace
        operator op = lexeme $ string op

        digit =
            QQ (\c -> fromIntegral (ord c) - 40)
                <$> oneOf "0123456789"
        number = lexeme $ QQ (Num) <$> somel (QQ (\q r -> 10 * q + r)) (LIFTED 0) digit

        atom = lexeme (char '(') *> sum' <* lexeme (char ')') <|> number
        sum' =
            precHomo
                atom
                [ ops
                    InfixL
                    [ operator "*" $> QQ (Bin Mul)
                    , operator "/" $> QQ (Bin Div)
                    ]
                , ops
                    InfixL
                    [ operator "+" $> QQ (Bin Add)
                    , operator "-" $> QQ (Bin Sub)
                    ]
                ]
