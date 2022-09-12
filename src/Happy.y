{
module Happy where

import Expr
import Alex

import qualified Data.ByteString.Lazy as BS
}

%name parse
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
  contents <- BS.readFile filepath
  let toks = alexScanTokens contents
  pure $ Just $ parse toks

parseError = error "parse error"
}
