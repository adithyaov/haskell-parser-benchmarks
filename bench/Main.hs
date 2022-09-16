{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Maybe (fromJust)
import Expr
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

import AlexHappy.Grammar qualified
import Attoparsec.ByteString qualified
import Attoparsec.Text qualified
import FlatParse qualified
import Handwritten.CPS qualified
import Handwritten.Normal qualified
import MegaHappy.Grammar qualified
import Megaparsec.ByteString qualified
import Megaparsec.Text qualified
import Parsec.ByteString qualified
import Parsec.Text qualified
import Parsley.ByteString qualified
import Parsley.Text qualified
import UUParsingLib qualified
import Streamly qualified


main :: IO ()
main =
    defaultMain [byteString, text]

byteString :: Benchmark
byteString =
            bgroup
                "ByteString"
                [ makeBench "Flatparse" FlatParse.parseFile
                , makeBench "Handwritten.CPS" Handwritten.CPS.parseFile
                , makeBench "Handwritten.Normal" Handwritten.Normal.parseFile
                , makeBench "Streamly" Streamly.parseFile
                , makeBench "Attoparsec" Attoparsec.ByteString.parseFile
                , makeBench "Megaparsec/Happy" MegaHappy.Grammar.parseFile
                , makeBench "Parsley" Parsley.ByteString.parseFile
                , makeBench "Megaparsec" Megaparsec.ByteString.parseFile
                , makeBench "Alex/Happy" AlexHappy.Grammar.parseFile
                , makeBench "Parsec" Parsec.ByteString.parseFile
                ]

text :: Benchmark
text = bgroup
                "Text"
                [ makeBench "Attoparsec" Attoparsec.Text.parseFile
                , makeBench "Parsley" Parsley.Text.parseFile
                , makeBench "Megaparsec" Megaparsec.Text.parseFile
                , makeBench "Parsec" Parsec.Text.parseFile
                , makeBench "UU Parsing Lib" UUParsingLib.parseFile
                ]

baselineBenchName = "Flatparse"
baselineBenchPat = printAwkExpr $ locateBenchmark [baselineBenchName]

makeBench :: String -> (FilePath -> IO (Maybe Expr)) -> Benchmark
makeBench name parseFile =
    let benchmark = bench name $ whnfIO $ fromJust <$> parseFile "big-example.txt"
     in if name == baselineBenchName
            then benchmark
            else bcompare baselineBenchPat benchmark
