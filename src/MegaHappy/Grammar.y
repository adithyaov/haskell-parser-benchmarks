{
module MegaHappy.Grammar (parseFile) where

import Expr
import MegaHappy.Parser

import qualified Data.ByteString as BS
import Text.Megaparsec (parseMaybe)
}

%name parser
%lexer { parseToken } { TEndOfFile }
%monad { Parser }
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
parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    content <- BS.readFile filepath
    pure $ parseMaybe parser content

parseError :: Token -> Parser a
parseError _ = fail "parse error"
}