{-# LANGUAGE StrictData #-}

module Expr where

import Data.Word


data Expr
    = Bin Op Expr Expr
    | Num Word64
    | Negate Expr


data Op
    = Add
    | Sub
    | Mul
    | Div
