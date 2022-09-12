{
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Alex where

import Data.Char (ord)
import Data.Word
import Expr
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "basic-bytestring"

$digit = 0-9

token :-
  $white+      ;
  \+           { \_ -> TOp Add }
  \-           { \_ -> TOp Sub }
  \*           { \_ -> TOp Mul }
  \/           { \_ -> TOp Div }
  \(           { \_ -> TParBeg }
  \)           { \_ -> TParEnd }
  $digit+      { TNum . BS.foldl' (\n c -> 10 * n + digit c) 0 }


{
data Token
  = TOp !Op
  | TParBeg
  | TParEnd
  | TNum !Word64

digit :: Char -> Word64
digit c = fromIntegral $ ord c - ord '0'
}

