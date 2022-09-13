{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Parsley.ByteString (parseFile) where

import Data.ByteString qualified as BS

import Expr
import Parsley (parse)
import Parsley.Parser


parseByteString :: BS.ByteString -> Maybe Expr
parseByteString = $$(parse expr)


parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    contents <- BS.readFile filepath
    pure $ parseByteString contents
