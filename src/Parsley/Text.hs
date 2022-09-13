{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Parsley.Text (parseFile) where

import Data.Text qualified as T
import Data.Text.IO qualified as T

import Expr
import Parsley (parse)
import Parsley.Parser


parseText :: T.Text -> Maybe Expr
parseText = $$(parse expr)


parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
    contents <- T.readFile filepath
    pure $ parseText contents
