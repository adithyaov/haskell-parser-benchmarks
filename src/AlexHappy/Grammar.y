{
module AlexHappy.Grammar (parseFile, parseString) where

import Expr
import AlexHappy.Lexer

import qualified Data.ByteString.Lazy.Char8 as BS
}

%name parser
%lexer { token } { TEndOfFile }
%monad { Alex }
%tokentype { Token }
%error { parseError }

%token
    '+' { TOp Add }
    '-' { TOp Sub }
    '*' { TOp Mul }
    '/' { TOp Div }
    '(' { TParBeg }
    ')' { TParEnd }
    num { TNum $$ }


%left '+' '-'
%left '*' '/'
%%

Expr : Expr '+' Expr  { Bin Add $1 $3 }
     | Expr '-' Expr  { Bin Sub $1 $3 }
     | Expr '*' Expr  { Bin Mul $1 $3 }
     | Expr '/' Expr  { Bin Div $1 $3 }
     | '(' Expr ')'   { $2 }
     | num            { Num $1 }

{

{-# INLINE token #-}
token :: (Token -> Alex a) -> Alex a
token = (alexMonadScan >>=)

parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
  contents <- BS.readFile filepath
  pure $ case runAlex contents parser of
    Left _ -> Nothing
    Right ans -> Just ans

parseString :: String -> Maybe Expr
parseString str =
    let input = BS.pack str
    in  case runAlex input parser of
            Left _ -> Nothing
            Right ans -> Just ans

parseError :: Token -> Alex a
parseError _ = alexError "parse error"
}
