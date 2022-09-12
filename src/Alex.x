{
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Alex (alexMonadScan, Token(..), alexError, Alex, runAlex) where

import Data.Char (ord)
import Data.Word
import Expr
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monad-bytestring"

$digit = 0-9

token :-
  $white+      ;
  \+           { \_ _ -> pure $ TOp Add }
  \-           { \_ _ -> pure $ TOp Sub }
  \*           { \_ _ -> pure $ TOp Mul }
  \/           { \_ _ -> pure $ TOp Div }
  \(           { \_ _ -> pure $ TParBeg }
  \)           { \_ _ -> pure $ TParEnd }
  $digit+      { mkT $ TNum . BS.foldl' (\n c -> 10 * n + digit c) 0 }


{
data Token
  = TOp !Op
  | TParBeg
  | TParEnd
  | TNum !Word64
  | TEndOfFile

digit :: Char -> Word64
digit c = fromIntegral $ ord c - ord '0'

{-# INLINE mkT #-}
mkT :: (BS.ByteString -> a) -> AlexInput -> Int64 -> Alex a
mkT f (_, _, str, _) len = pure $ f $ BS.take len str

alexEOF :: Alex Token
alexEOF = pure TEndOfFile
}

